package edu.evolution.varanovich.anki.client

import cats.effect.IO
import edu.evolution.varanovich.anki.api.http.protocol.AnkiRequest.{AnkiGenericRequest, CreateAnswerInfoRequest, DeckRequest, UserRequest}
import edu.evolution.varanovich.anki.model.{AnswerInfo, Card}
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.{Header, Method, Request}

trait AnkiHttpRequest {
  def send(implicit cookies: UserCookies): Request[IO]
}
object AnkiHttpRequest {
  final case class RegisterRequest(name: String, password: String) extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.POST)
      .withUri(Uri)
      .withPathInfo("/register")
      .withEntity(UserRequest(name, password))
  }
  final case class LoginRequest(name: String, password: String) extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.POST)
      .withUri(Uri)
      .withPathInfo("/login")
      .withEntity(UserRequest(name, password))
  }
  final case class GenerateDeckRequest(size: Int) extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.GET)
      .withUri(Uri)
      .withPathInfo(s"/deck/$size")
      .withHeaders(
        Header.apply("user-id", cookies.id),
        Header.apply("token", cookies.token))
  }
  final case class LastDeckRequest() extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.GET)
      .withUri(Uri)
      .withPathInfo(s"/last-generated-deck")
      .withHeaders(
        Header.apply("user-id", cookies.id),
        Header.apply("token", cookies.token))
  }
  final case class CreateDeckRequest(description: String, cards: Array[Card]) extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.POST)
      .withUri(Uri)
      .withPathInfo("/save-deck")
      .withHeaders(
        Header.apply("user-id", cookies.id),
        Header.apply("token", cookies.token))
      .withEntity(DeckRequest(description, cards))
  }
  final case class LastDeckByPatternRequest(pattern: String) extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.POST)
      .withUri(Uri)
      .withPathInfo("/last-deck")
      .withHeaders(
        Header.apply("user-id", cookies.id),
        Header.apply("token", cookies.token))
      .withEntity(AnkiGenericRequest(pattern))
  }
  final case class SaveAnswerInfoRequest(identity: String, card: Card, info: AnswerInfo, isImprove: Boolean)
    extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = {
      val path: String = if (isImprove) "improve-answer-info" else "save-answer-info"
      new Request()
        .withMethod(Method.POST)
        .withUri(Uri)
        .withPathInfo(s"/$path")
        .withHeaders(
          Header.apply("user-id", cookies.id),
          Header.apply("token", cookies.token))
        .withEntity(CreateAnswerInfoRequest(identity, card, info))
    }
  }
  final case class EarliestFreshDeckRequest() extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.GET)
      .withUri(Uri)
      .withPathInfo(s"/earliest-fresh-deck")
      .withHeaders(
        Header.apply("user-id", cookies.id),
        Header.apply("token", cookies.token))
  }
  final case class CardsForImproveRequest(limit: Int) extends AnkiHttpRequest {
    override def send(implicit cookies: UserCookies): Request[IO] = new Request()
      .withMethod(Method.GET)
      .withUri(Uri)
      .withPathInfo(s"/cards-for-improve/$limit")
      .withHeaders(
        Header.apply("user-id", cookies.id),
        Header.apply("token", cookies.token))
  }
}