package edu.evolution.varanovich.anki.client

import java.util.concurrent.TimeUnit

import edu.evolution.varanovich.anki.adt.{Card, Deck}

class UserCookies {
  var id: String = ""
  var token: String = ""
  var deck: Deck = Deck(Set(), "deck")
  var cardList: List[Card] = List()
  var timestamp: Long = 0L
  var lastAnswerDurationSec: Short = 0

  def updateCredentials(id: String, token: String): Unit = {
    this.id = id
    this.token = token
  }

  def updateDeck(deck: Deck): Unit = {
    this.deck = deck
    this.cardList = deck.cards.toList
  }

  def updateTimeStamp(timestamp: Long): Unit = {
    this.timestamp = timestamp
  }

  def calculateDurationSec(timestamp: Long): Unit = {
    val durationMillis = timestamp - this.timestamp
    val durationSec: Long = TimeUnit.MILLISECONDS.toSeconds(durationMillis)
    this.lastAnswerDurationSec = durationSec.toShort
  }
}