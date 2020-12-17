package edu.evolution.varanovich.anki.client

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import edu.evolution.varanovich.anki.deckOptThird
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class UserCookiesSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "Should successfully store deck" in {
    val userCookies = new UserCookies()
    for {
      deck <- IO.fromOption(deckOptThird)(throw new Exception("Deck not created"))
      _ <- IO(userCookies.updateDeck(deck)) *> IO.unit
      cards <- IO(userCookies.deck.cards)
    } yield assert(cards.size == deck.cards.size)
  }
}
