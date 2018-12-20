package com.lambdaminute.tapir.http4s
import java.nio.charset.StandardCharsets.UTF_8

import cats.Applicative
import cats.data._
import cats.effect.Sync
import cats.implicits._
import io.circe.{Encoder, Json}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Headers, HttpRoutes, Request, Response}
import tapir.typelevel.ParamsAsArgs
import tapir.{DecodeResult, Endpoint, EndpointIO, EndpointInput}

import scala.language.higherKinds

object SeqToParams {
  def apply[T](seq: Seq[T]): Any =
    seq match {
      case Seq()                       => ()
      case Seq(v)                      => v
      case Seq(v1, v2)                 => (v1, v2)
      case Seq(v1, v2, v3)             => (v1, v2, v3)
      case Seq(v1, v2, v3, v4)         => (v1, v2, v3, v4)
      case Seq(v1, v2, v3, v4, v5)     => (v1, v2, v3, v4, v5)
      case Seq(v1, v2, v3, v4, v5, v6) => (v1, v2, v3, v4, v5, v6)
      case _                           => throw new IllegalArgumentException(s"Cannot convert $seq to params!")
    }
}

trait Http4sInterpreter {

  private val logger = org.log4s.getLogger

  implicit class RichHttp4sHttpEndpoint[I, E, O: Encoder](e: Endpoint[I, E, O]) {

    case class MatchResult[F[_]](values: List[Any], ctx: Context[F]) {
      def prependValue(v: Any): MatchResult[F] = copy(values = v :: values)
    }

    type Error = String

    def toHttp4sService[F[_]: Sync, FN[_]](logic: FN[F[Either[E, O]]])(
        implicit paramsAsArgs: ParamsAsArgs.Aux[I, FN]): HttpRoutes[F] = {

      val inputs = e.input.asVectorOfSingle

      logger.debug(s"Inputs: ")
      logger.debug(inputs.mkString("\n"))

      val service: HttpRoutes[F] = HttpRoutes[F] { req: Request[F] =>
        val context: F[Context[F]] =
          req.bodyAsText.compile.last.map(
            maybeBody =>
              Context[F](queryParams = req.params,
                         headers = req.headers,
                         body = maybeBody,
                         unmatchedPath = req.uri.renderString))

        val response: ContextState[F] = matchInputs[F](inputs)

        val value: F[Either[Error, (Context[F], MatchResult[F])]] = context.map(response.run)

        logger.debug(s"Result of binding: ${value}")

        val maybeMatch: OptionT[F, I] = OptionT(value.map(_.toOption.map {
          case (context, result) =>
            logger.debug(s"Result of binding: ${result.values}")
            logger.debug(context.toString)
            SeqToParams(result.values).asInstanceOf[I]
        }))

        val res: OptionT[F, F[Either[E, O]]] = maybeMatch.map(i => paramsAsArgs.applyFn(logic, i))

        res.flatMapF(_.map {
          case Right(result) =>
            val encode: Json = Encoder[O].apply(result)
            Option(Response[F](body = fs2.Stream.fromIterator(encode.noSpaces.getBytes(UTF_8).iterator)))
          case Left(err) =>
            logger.error(err.toString)
            None
        })
      }

      service

    }

    def nextSegment(unmatchedPath: String): Either[Error, (String, String)] =
      if (unmatchedPath.startsWith("/")) {
        val noSlash = unmatchedPath.drop(1)
        val lastIndex =
          if (noSlash.contains("/")) noSlash.indexOf("/")
          else if (noSlash.contains("?")) noSlash.indexOf("?")
          else
            noSlash.length - 1
        val (segment, remaining) = noSlash.splitAt(lastIndex)
        Either.right((segment, remaining))
      } else {
        Either.left("path doesn't start with \"/\"")
      }

    case class Context[F[_]](queryParams: Map[String, String],
                             headers: Headers,
                             body: Option[String],
                             unmatchedPath: String) {
      def getHeader(key: String): Option[String]      = headers.get(CaseInsensitiveString.apply(key)).map(_.value)
      def getQueryParam(name: String): Option[String] = queryParams.get(name)
      def dropPath(n: Int): Context[F]                = copy(unmatchedPath = unmatchedPath.drop(n))
    }

    implicit class VectorOfSingle[_](ei: EndpointInput[_]) {
      def asVectorOfSingle: Vector[EndpointInput.Single[_]] = ei match {
        case s: EndpointInput.Single[_]   => Vector(s)
        case m: EndpointInput.Multiple[_] => m.inputs
        case m: EndpointIO.Multiple[_]    => m.ios
      }
    }

    def handleMapped[II, T, F[_]: Sync](wrapped: EndpointInput[II],
                                        f: II => T,
                                        inputsTail: Vector[EndpointInput.Single[_]]): ContextState[F] =
      for {
        r1 <- matchInputs[F](wrapped.asVectorOfSingle)
        r2 <- matchInputs[F](inputsTail)
          .map(_.prependValue(f.asInstanceOf[Any => Any].apply(SeqToParams(r1.values))))
      } yield r2

    private def continueMatch[F[_]: Sync](decodeResult: DecodeResult[Any],
                                          inputsTail: Vector[EndpointInput.Single[_]]): ContextState[F] =
      decodeResult match {
        case DecodeResult.Value(v) =>
          logger.debug(s"Continuing match: ${v}")
          matchInputs[F](inputsTail).map(_.prependValue(v))
        case err =>
          StateT.inspectF((ctx: Context[F]) => Either.left(s"${err.toString}, ${ctx.unmatchedPath}"))
      }

    type ContextState[F[_]] = StateT[Either[Error, ?], Context[F], MatchResult[F]]
    private def getState[F[_]]: StateT[Either[Error, ?], Context[F], Context[F]] = StateT.get
    private def modifyState[F[_]: Applicative](
        f: Context[F] => Context[F]): StateT[Either[Error, ?], Context[F], Unit] =
      StateT.modify[Either[Error, ?], Context[F]](f)

    def matchInputs[F[_]: Sync](inputs: Vector[EndpointInput.Single[_]]): ContextState[F] = inputs match {
      case Vector() =>
        StateT(context => Either.right(context, MatchResult[F](Nil, context)))
      case EndpointInput.PathSegment(ss: String) +: inputsTail =>
        for {
          ctx <- getState[F]
          _   <- modifyState[F](_.dropPath(ss.length + 1))
          doesMatch = ctx.unmatchedPath.drop(1).startsWith(ss)
          _         = logger.debug(s"${doesMatch}, ${ctx.unmatchedPath}, ${ss}")
          r <- if (ctx.unmatchedPath.drop(1).startsWith(ss)) {
            logger.debug(s"Matched path: ${ss}, $ctx")
            val value: ContextState[F] = matchInputs[F](inputsTail)
            value
          } else {
            val value: ContextState[F] = StateT.liftF(Either.left(s"Unmatched path segment: ${ss}, ${ctx}"))
            value
          }
        } yield r
      case capture @ EndpointInput.PathCapture(m, name, _, _) +: inputsTail =>
        val decodeResult: StateT[Either[Error, ?], Context[F], DecodeResult[Any]] = StateT(
          (ctx: Context[F]) =>
            nextSegment(ctx.unmatchedPath)
              .map {
                case (segment, remaining) =>
                  logger.debug(s"Capturing path: ${segment}, remaining: ${remaining}, ${name}")
                  (ctx.copy(unmatchedPath = remaining), m.fromString(segment))
            })

        decodeResult.flatMap {
          case DecodeResult.Value(v) =>
            logger.debug(s"Decoded path: ${v}")
            matchInputs[F](inputsTail).map(_.prependValue(v))
          case decodingFailure => StateT.liftF(Either.left(s"Decoding path failed: $decodingFailure"))
        }
      case q @ EndpointInput.Query(name, m, _, _) +: inputsTail =>
        for {
          ctx <- getState[F]
          query = m.fromOptionalString(ctx.getQueryParam(name))
          _     = logger.debug(s"Found query: ${query}, ${name}, ${ctx.headers}")
          res <- continueMatch(query, inputsTail)
        } yield res
      case EndpointIO.Header(name, m, _, _) +: inputsTail =>
        for {
          ctx <- getState[F]
          header = m.fromOptionalString(ctx.getHeader(name))
          _      = logger.debug(s"Found header: ${header}")
          res <- continueMatch(header, inputsTail)
        } yield res
      case EndpointIO.Body(m, _, _) +: inputsTail =>
        for {
          ctx <- getState[F]
          res <- continueMatch(m.fromOptionalString(ctx.body), inputsTail)
        } yield res
      case EndpointInput.Mapped(wrapped, f, _, _) +: inputsTail =>
        handleMapped(wrapped, f, inputsTail)
      case EndpointIO.Mapped(wrapped, f, _, _) +: inputsTail =>
        handleMapped(wrapped, f, inputsTail)
    }

  }

}
