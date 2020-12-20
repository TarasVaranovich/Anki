package edu.evolution.varanovich.anki.client

import java.util.concurrent.TimeUnit

import edu.evolution.varanovich.anki.adt.{Card, Deck}

class UserCookies {
  var id: String = ""
  var token: String = ""
  var deck: Deck = Deck(Set(), "deck")
  var cardList: List[Card] = List()
  var cardAliasList: List[String] = List()
  var timestamp: Long = 0L
  var lastAnswerDurationSec: Short = 0
  private var temporaryCardSet: Boolean = false

  def updateCredentials(id: String, token: String): Unit = {
    this.id = id
    this.token = token
  }

  def updateDeck(deck: Deck): Unit = {
    this.deck = deck
    this.cardList = deck.cards.toList
    this.temporaryCardSet = false
  }

  def updateTemporaryDeck(cardMap: Map[String, Card]): Unit = {
    val cardTuples: List[(String, Card)] = cardMap.toList
    this.cardList = cardTuples.map { case (_, card) => card }
    this.cardAliasList = cardTuples.map { case (uuid, _) => uuid }
    this.temporaryCardSet = true
  }

  def updateTimeStamp(timestamp: Long): Unit = {
    this.timestamp = timestamp
  }

  def calculateDurationSec(timestamp: Long): Unit = {
    val durationMillis = timestamp - this.timestamp
    val durationSec: Long = TimeUnit.MILLISECONDS.toSeconds(durationMillis)
    this.lastAnswerDurationSec = durationSec.toShort
  }

  def isTemporaryCardSet: Boolean = temporaryCardSet

  def resolveCardIdentity(card: Card): String = {
    val cardIndex = this.cardList.indexOf(card)
    this.cardAliasList(cardIndex)
  }
}