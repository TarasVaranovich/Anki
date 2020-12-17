package edu.evolution.varanovich.anki.api.http.protocol

import cats.syntax.functor._
import edu.evolution.varanovich.anki.adt.Deck
import io.circe.Decoder
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec

trait AnkiResponse
object AnkiResponse {
  final case class AnkiGenericResponse(message: String) extends AnkiResponse
  final case class ErrorResponse(message: String) extends AnkiResponse
  final case class MultiErrorResponse(messages: Array[String]) extends AnkiResponse
  final case class DeckResponse(deck: Deck) extends AnkiResponse
  final case class UserResponse(id: String, token: String) extends AnkiResponse

  implicit val decodeAnkiResponse: Decoder[AnkiResponse] =
    List[Decoder[AnkiResponse]](
      Decoder[ErrorResponse].widen,
      Decoder[MultiErrorResponse].widen,
      Decoder[DeckResponse].widen,
      Decoder[UserResponse].widen
    ).reduceLeft(_ or _)
}
