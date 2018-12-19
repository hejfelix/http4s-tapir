package com.lambdaminute.tapir.http4s
import java.nio.charset.StandardCharsets.UTF_8

import cats.data.{EitherT, OptionT}
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
        val context = Context(queryParams = req.params,
                              headers = req.headers,
                              body = req.as[String],
                              unmatchedPath = req.uri.renderString)

        logger.debug(s"Context: ")
        logger.debug(context.toString)
        val response: EitherT[F, Error, MatchResult[F]] = matchInputs(inputs)(context)

        val options: OptionT[F, I] = response.toOption.map { result =>
          logger.debug(s"Result of binding: ${result.values}")
          SeqToParams(result.values).asInstanceOf[I]
        }

        val res: OptionT[F, F[Either[E, O]]] = options.map(i => paramsAsArgs.applyFn(logic, i))

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

    def getQueryParams(unmatchedPath: String): Either[Error, Map[String, String]] =
      Either.cond(
        unmatchedPath.startsWith("?"),
        unmatchedPath
          .drop(1)
          .split("&")
          .map(s => s.splitAt(s.indexOf("=")))
          .toMap,
        s"Couldn't parse query params: $unmatchedPath"
      )

    case class Context[F[_]](queryParams: Map[String, String],
                             headers: Headers,
                             body: F[String],
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

    def handleMapped[II, T, F[_]: Sync](
        wrapped: EndpointInput[II],
        f: II => T,
        inputsTail: Vector[EndpointInput.Single[_]])(ctx: Context[F]): EitherT[F, Error, MatchResult[F]] =
      matchInputs[F](wrapped.asVectorOfSingle)(ctx).flatMap { result: MatchResult[F] =>
        matchInputs[F](inputsTail)(result.ctx).map(
          _.prependValue(f.asInstanceOf[Any => Any].apply(SeqToParams(result.values)))
        )
      }

    private def continueMatch[F[_]: Sync](decodeResult: DecodeResult[Any], inputsTail: Vector[EndpointInput.Single[_]])(
        ctx: Context[F]): EitherT[F, Error, MatchResult[F]] =
      decodeResult match {
        case DecodeResult.Value(v) =>
          logger.debug(s"Continuing match: ${v}")
          matchInputs(inputsTail)(ctx).map(_.prependValue(v))
        case err =>
          EitherT.leftT(s"${err.toString}, ${ctx.unmatchedPath}")
      }

    def matchInputs[F[_]: Sync](inputs: Vector[EndpointInput.Single[_]])(
        ctx: Context[F]): EitherT[F, Error, MatchResult[F]] =
      inputs match {
        case Vector() => EitherT.rightT[F, Error](MatchResult(Nil, ctx))
        case EndpointInput.PathSegment(ss: String) +: inputsTail if ctx.unmatchedPath.drop(1).startsWith(ss) =>
          logger.debug(s"Matched ${ss}")
          matchInputs(inputsTail)(ctx.dropPath(ss.length + 1))
        case capture @ EndpointInput.PathCapture(m, name, _, _) +: inputsTail =>
          logger.debug(s"Capturing path ${name.mkString}: $capture\n${ctx} ")
          EitherT
            .fromEither[F] {
              val next: Either[Error, (DecodeResult[Any], String)] = nextSegment(ctx.unmatchedPath).map {
                case (segment, remaining) =>
                  (m.fromString(segment), remaining)
              }
              logger.debug(s"Next segment ${name.mkString}: ${next}, ${ctx.unmatchedPath}")
              next
            }
            .flatMap {
              case (DecodeResult.Value(v), remaining) =>
                logger.debug(s"Decoded path: ${v}, remaining: ${remaining}")
                matchInputs(inputsTail)(ctx.copy(unmatchedPath = remaining)).map {
                  _.prependValue(v)
                }
              case decodingFailure =>
                logger.debug(s"Decode failure ${name.mkString}: ${decodingFailure}")
                EitherT.leftT(s"Decoding path failed: $decodingFailure")
            }
        case q @ EndpointInput.Query(name, m, _, _) +: inputsTail =>
          logger.debug(s"Capturing query ${name}: $q")
          continueMatch(m.fromOptionalString(ctx.queryParams.get(name)), inputsTail)(ctx)
        case EndpointIO.Header(name, m, _, _) +: inputsTail =>
          logger.debug(s"Matching header: ${name}")
          continueMatch(m.fromOptionalString(ctx.getHeader(name)), inputsTail)(ctx)
        case EndpointIO.Body(m, _, _) +: inputsTail =>
          val body = EitherT.right[Error](ctx.body)
          body.flatMap(
            bodyAsString => continueMatch(m.fromOptionalString(Option(bodyAsString)), inputsTail)(ctx)
          )
        case EndpointInput.Mapped(wrapped, f, _, _) +: inputsTail =>
          handleMapped(wrapped, f, inputsTail)(ctx)
        case EndpointIO.Mapped(wrapped, f, _, _) +: inputsTail =>
          handleMapped(wrapped, f, inputsTail)(ctx)
        case notMatched =>
          logger.debug(s"Could not match: ${inputs}")
          logger.debug(s"Context: ${ctx}")
          ???
      }

  }

}
