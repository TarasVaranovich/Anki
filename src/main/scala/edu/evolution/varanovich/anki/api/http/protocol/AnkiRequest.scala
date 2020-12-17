package edu.evolution.varanovich.anki.api.http.protocol

import edu.evolution.varanovich.anki.adt.Card

trait AnkiRequest
object AnkiRequest {
  final case class AnkiGenericRequest(data: String) extends AnkiRequest
  final case class DeckRequest(description: String, cards: Array[Card]) extends AnkiRequest
  final case class UserRequest(name: String, password: String) extends AnkiRequest
}
