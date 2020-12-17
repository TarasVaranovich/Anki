package edu.evolution.varanovich.anki.client

import edu.evolution.varanovich.anki.adt.Deck

class UserCookies {
  var id: String = ""
  var token: String = ""
  var deck: Deck = Deck(Set(), "deck")

  def updateCredentials(id: String, token: String): Unit = {
    this.id = id
    this.token = token
  }

  def updateDeck(deck: Deck): Unit = {
    this.deck = deck
  }
}