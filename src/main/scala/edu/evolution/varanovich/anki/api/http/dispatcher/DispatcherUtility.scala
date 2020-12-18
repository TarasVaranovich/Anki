package edu.evolution.varanovich.anki.api.http.dispatcher

import cats.effect.IO
import edu.evolution.varanovich.anki.api.http.protocol.AnkiResponse.ErrorResponse
import edu.evolution.varanovich.anki.api.session.Session.Cache
import edu.evolution.varanovich.anki.api.session.UserSession
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Header, Request, Response, Status}

object DispatcherUtility {
  private val unauthorizedResponse: Response[IO] =
    Response(Status.Accepted).withEntity(ErrorResponse(s"You are not logged in."))

  def executeAuthenticated(request: Request[IO],
                           cache: Cache[IO, String, UserSession],
                           function: String => IO[Response[IO]]): IO[Response[IO]] = {
    val userIdHeaderOpt: Option[Header] = request.headers.get(CaseInsensitiveString.apply("user-id"))
    val tokenHeaderOpt: Option[Header] = request.headers.get(CaseInsensitiveString.apply("token"))
    (userIdHeaderOpt, tokenHeaderOpt) match {
      case (Some(userIdHeader), Some(tokenHeader)) =>
        for {
          tokenOpt <- cache.get(userIdHeader.value).map(_.map(_.token))
          response <- tokenOpt match {
            case Some(token) => if (token == tokenHeader.value) function.apply(userIdHeader.value)
            else IO(unauthorizedResponse)
            case None => IO(unauthorizedResponse)
          }
        } yield response
      case (_, _) => IO(unauthorizedResponse)
    }
  }
}
