package com.lambdaminute.tapir.http4s
import cats.data.{Kleisli, OptionT}
import cats.effect.{ExitCode, Fiber, IO, IOApp}
import tapir._
import tapir.json.circe._
import io.circe.generic.auto._
import org.http4s.{Http, HttpApp, HttpRoutes}
import org.http4s.server.blaze._
import cats.effect.ContextShift
import concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import fs2.Stream
import org.http4s
import org.http4s.implicits._

object Test extends IOApp with Http4sInterpreter {
  type Limit     = Int
  type AuthToken = String
  case class BooksFromYear(genre: String, year: Int)
  case class Book(title: String)

  val booksListing: Endpoint[(BooksFromYear, Limit, AuthToken), String, List[Book]] = endpoint.get
    .in(("books" / path[String]("genre") / path[Int]("year")).mapTo(BooksFromYear))
    .in(query[Int]("limit").description("Maximum number of books to retrieve"))
    .in(header[String]("X-Auth-Token"))
    .errorOut(stringBody)
    .out(jsonBody[List[Book]])

//

  import tapir.docs.openapi._
  import tapir.openapi.circe.yaml._

  val docs = booksListing.toOpenAPI("My Bookshop", "1.0")
  println(docs.toYaml)

  import tapir.client.sttp._
  import com.softwaremill.sttp._

  implicit val backend = HttpURLConnectionBackend()
  val booksListingRequest: Request[Either[String, List[Book]], Nothing] = booksListing
    .toSttpRequest(uri"http://localhost:8080")
    .apply(BooksFromYear("SF", 2016), 20, "xyz-abc-123")

  def bookListingLogic(bfy: BooksFromYear, limit: Limit, at: AuthToken): IO[Either[String, List[Book]]] =
    IO.pure(Right(List(Book("The Sorrows of Young Werther"))))

  val service: HttpRoutes[IO] = booksListing.toHttp4sService(bookListingLogic _)

  val app: HttpApp[IO]            = org.http4s.server.Router("/" -> service).orNotFound
  val res: BlazeServerBuilder[IO] = BlazeServerBuilder[IO].bindHttp(8080, "localhost").withHttpApp(app)

  private val cs = IO.contextShift(concurrent.ExecutionContext.Implicits.global)

  private def serve: IO[Fiber[IO, ExitCode]] =
    res.serve.head.compile.lastOrError.start(cs)

  private def request: IO[Fiber[IO, Unit]] = {
    import concurrent.duration._

    IO.sleep(2.seconds) *>
      IO.delay(booksListingRequest.send)
        .map(res => println(res.body.flatten))
        .start(cs)
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      serverThread  <- serve
      requestThread <- request
      _             <- requestThread.join
      exitCode      <- serverThread.join
    } yield exitCode

  org.log4s.getLogger.info("HEJ")

}
