package edu.evolution.varanovich.anki.api.http

import cats.effect.{ExitCode, IO, IOApp, Resource}
import doobie.Transactor
import edu.evolution.varanovich.anki.api.http.dispatcher.{CardDispatcher, DeckDispatcher, UserDispatcher}
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.ErrorResponse
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.config.{AnkiConfig, ServerConfig}
import edu.evolution.varanovich.anki.db.DbManager
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io.{->, /, GET, POST, Root}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, Response, Status}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object AnkiServer extends IOApp {
  private val config = ServerConfig.load
  val ServerErrorResponse: Response[IO] =
    Response(Status.Accepted).withEntity(ErrorResponse("Server error."))

  def ankiRoutes(cache: Cache[IO, String, UserSession], transactor: Resource[IO, Transactor[IO]]) = {
    val currentLogger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]
    val config = AnkiConfig.load
    implicit val currentCache: Cache[IO, String, UserSession] = cache
    implicit val currentTransactor: Resource[IO, Transactor[IO]] = transactor

    val cardDispatcher = CardDispatcher(currentLogger)
    val deckDispatcher = DeckDispatcher(currentLogger, config)
    val userDispatcher = UserDispatcher(currentLogger)

    HttpRoutes.of[IO] {
      case request@POST -> Root / "register" => userDispatcher.doRegister(request)
      case request@POST -> Root / "login" => userDispatcher.doLogin(request)
      case request@GET -> Root / "deck" / size => deckDispatcher.doRandom(size, request)
      case request@GET -> Root / "last-generated-deck" => deckDispatcher.doLastGenerated(request)
      case request@POST -> Root / "save-deck" => deckDispatcher.doSave(request)
      case request@POST -> Root / "last-deck" => deckDispatcher.doLastByPattern(request)
      case request@POST -> Root / "save-answer-info" => cardDispatcher.createAnswerInfo(request)
      case request@GET -> Root / "earliest-fresh-deck" => deckDispatcher.doEarliestFresh(request)
      case request@GET -> Root / "cards-for-improve" / limit => cardDispatcher.doCardsForImprove(request, limit)
      case request@POST -> Root / "improve-answer-info" => cardDispatcher.improveAnswerInfo(request)
    }
  }

  def httpApp(cache: Cache[IO, String, UserSession], transactor: Resource[IO, Transactor[IO]]) = {
    ankiRoutes(cache, transactor)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      transactor <- IO(DbManager.transactorInstance)
      cache <- Cache.of[IO, String, UserSession](config.cacheExpiration.minutes, config.cacheInvalidation.minutes)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = config.port, host = config.host)
        .withHttpApp(httpApp(cache, transactor))
        .serve
        .compile
        .drain
    } yield ExitCode.Success
}