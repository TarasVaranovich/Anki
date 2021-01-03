package edu.evolution.varanovich.anki.domain

import cats.effect.testing.scalatest.AsyncIOSpec
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.domain.domain.RandomDeckDescriptionLength
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers

class DeckBuilderSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private val config = AnkiConfig.load
  private implicit val transactor = DbManager.transactorInstance
  private val deckBuilder = DeckBuilder(config)

  "should successfully generate random deck with max size" in {
    deckBuilder.randomDeck(config.maxDeckLength).map(deck => {
      assert(deck.isDefined)
      assert(deck.get.cards.size == config.maxDeckLength)
      assert(deck.get.description.length == RandomDeckDescriptionLength)
    })
  }

  "should successfully generate random deck with min size" in {
    deckBuilder.randomDeck(config.minDeckLength).map(deck => {
      assert(deck.isDefined)
      assert(deck.get.cards.size == config.minDeckLength)
      assert(deck.get.description.length == RandomDeckDescriptionLength)
    })
  }

  "should not generate random deck with to small size" in {
    deckBuilder.randomDeck(3).map(deck => assert(deck.isEmpty))
  }

  "should not generate random deck if deck size greater than summary words count" in {
    deckBuilder.randomDeck(100000).map(deck => assert(deck.isEmpty))
  }

  "should not generate random deck if one of part of speech's counts can't fill required selection" in {
    deckBuilder.randomDeck(400).map(deck => assert(deck.isEmpty))
  }
}