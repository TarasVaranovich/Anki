package edu.evolution.varanovich.anki.domain

import cats.effect.testing.scalatest.AsyncIOSpec
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.domain.domain.RandomDeckDescriptionLength
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers

class DeckBuilderSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private val config = AnkiConfig.load
  "should successfully generate random deck with max size" in {
    DeckBuilder.randomDeck(config.maxDeckLength).map(deck => {
      assert(deck.isDefined)
      assert(deck.get.cards.size == config.maxDeckLength)
      assert(deck.get.description.length == RandomDeckDescriptionLength)
    })
  }

  "should successfully generate random deck with min size" in {
    DeckBuilder.randomDeck(config.minDeckLength).map(deck => {
      assert(deck.isDefined)
      assert(deck.get.cards.size == config.minDeckLength)
      assert(deck.get.description.length == RandomDeckDescriptionLength)
    })
  }

  "should not generate random deck with to small size" in {
    DeckBuilder.randomDeck(3).map(deck => assert(deck.isEmpty))
  }

  "should not generate random deck if deck size greater than summary words count" in {
    DeckBuilder.randomDeck(100000).map(deck => assert(deck.isEmpty))
  }

  "should not generate random deck if one of part of speech's counts can't fill required selection" in {
    DeckBuilder.randomDeck(400).map(deck => assert(deck.isEmpty))
  }
}