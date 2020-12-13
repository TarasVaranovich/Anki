package edu.evolution.varanovich.anki.domain

import cats.effect.testing.scalatest.AsyncIOSpec
import edu.evolution.varanovich.anki.db.program.domain.RandomDeckDescriptionLength
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxDeckLength, MinDeckLength}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers

class DeckBuilderSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully generate random deck with max size" in {
    for {
      deck <- DeckBuilder.randomDeck(MaxDeckLength)
    } yield {
      assert(deck.isDefined)
      assert(deck.get.cards.size == MaxDeckLength)
      assert(deck.get.description.length == RandomDeckDescriptionLength)
    }
  }

  "should successfully generate random deck with min size" in {
    for {
      deck <- DeckBuilder.randomDeck(MinDeckLength)
    } yield {
      assert(deck.isDefined)
      assert(deck.get.cards.size == MinDeckLength)
      assert(deck.get.description.length == RandomDeckDescriptionLength)
    }
  }

  "should not generate random deck with to small size" in {
    for {
      deck <- DeckBuilder.randomDeck(3)
    } yield {
      assert(deck.isEmpty)
    }
  }

  "should not generate random deck if deck size greater than summary words count" in {
    for {
      deck <- DeckBuilder.randomDeck(100000)
    } yield {
      assert(deck.isEmpty)
    }
  }

  "should not generate random deck if one of part of speech's counts can't fill required selection" in {
    for {
      deck <- DeckBuilder.randomDeck(400)
    } yield {
      assert(deck.isEmpty)
    }
  }
}