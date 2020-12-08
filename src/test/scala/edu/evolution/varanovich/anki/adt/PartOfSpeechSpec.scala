package edu.evolution.varanovich.anki.adt

import edu.evolution.varanovich.anki._
import org.scalatest.flatspec.AnyFlatSpec

class PartOfSpeechSpec extends AnyFlatSpec {
  it should "create Adjective successfully" in {
    assert(highAdjectiveOpt.isDefined)
  }

  it should "create Noun successfully" in {
    assert(coastNounOpt.isDefined)
  }

  it should "create Phrase successfully" in {
    assert(howAreYouPhraseOpt.isDefined)
  }

  it should "create Preposition successfully" in {
    assert(abovePrepositionOpt.isDefined)
  }

  it should "create Verb successfully" in {
    assert(consistVerbOpt.isDefined)
  }

  "Same nouns" should "be equal" in {
    for {
      noun <- coastNounOpt
      duplicate <- coastNounOpt
    } yield assert(noun.equals(duplicate))
  }

  "Different adjectives" should "be not equal" in {
    for {
      clumsy <- clumsyAdjectiveOpt
      deceiving <- bigAdjectiveOpt
    } yield assert(!clumsy.equals(deceiving))
  }

  it should "sort parts of speech by value" in {
    val words = List(highAdjectiveOpt, coastNounOpt, howAreYouPhraseOpt, abovePrepositionOpt, consistVerbOpt,
      clumsyAdjectiveOpt, bigAdjectiveOpt)
    val values: List[PartOfSpeech] = for {
      word <- words
      value <- word
    } yield value
    assert(values.sorted.map(_.getValue) == List("above", "big", "clumsy", "coast", "consist", "high", "How are you?"))
  }

  "Preposition" should "be init successfully with multi-translation" in {
    assert(betweenPrepositionOpt.isDefined)
  }

  "Noun" should "be init successfully with empty plural" in {
    assert(glassesNounOpt.isDefined)
  }

  "Noun" should "be init successfully with spaced plural" in {
    assert(glassesNounWithGapsOpt.isDefined)
  }
}