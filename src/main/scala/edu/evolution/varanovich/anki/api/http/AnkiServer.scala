package edu.evolution.varanovich.anki.api.http

import cats.effect.{ExitCode, IO, IOApp}
import edu.evolution.varanovich.anki.api.http.dispatcher.{CardDispatcher, DeckDispatcher, UserDispatcher}
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.ErrorResponse
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import edu.evolution.varanovich.anki.config.ServerConfig
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

  def ankiRoutes(cache: Cache[IO, String, UserSession]) = {
    HttpRoutes.of[IO] {
      case request@POST -> Root / "register" => UserDispatcher.doRegister(request, cache)
      case request@POST -> Root / "login" => UserDispatcher.doLogin(request, cache)
      case request@GET -> Root / "deck" / size => DeckDispatcher.doRandom(size, request, cache)
      case request@GET -> Root / "last-generated-deck" => DeckDispatcher.doLastGenerated(request, cache)
      case request@POST -> Root / "save-deck" => DeckDispatcher.doSave(request, cache)
      case request@POST -> Root / "last-deck" => DeckDispatcher.doLastByPattern(request, cache)
      case request@POST -> Root / "save-answer-info" => CardDispatcher.createAnswerInfo(request, cache)
      case request@GET -> Root / "earliest-fresh-deck" => DeckDispatcher.doEarliestFresh(request, cache)
      case request@GET -> Root / "cards-for-improve" / limit => CardDispatcher.doCardsForImprove(request, cache, limit)
      case request@POST -> Root / "improve-answer-info" => CardDispatcher.improveAnswerInfo(request, cache)
    }
  }

  def httpApp(cache: Cache[IO, String, UserSession]) = {
    ankiRoutes(cache)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      cache <- Cache.of[IO, String, UserSession](config.cacheExpiration.minutes, config.cacheInvalidation.minutes)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = config.port, host = config.host)
        .withHttpApp(httpApp(cache))
        .serve
        .compile
        .drain
    } yield ExitCode.Success
}