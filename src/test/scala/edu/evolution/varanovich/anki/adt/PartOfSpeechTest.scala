package edu.evolution.varanovich.anki.adt

import edu.evolution.varanovich.anki.adt.PartOfSpeech.{Preposition, _}
import org.scalatest.flatspec.AnyFlatSpec

class PartOfSpeechTest extends AnyFlatSpec {
  it should "create Adjective successfully" in {
    assert(Adjective.from("high", "высокий", "[hai]",
      "higher", "highest").isDefined)
  }

  it should "create Noun successfully" in {
    assert(Noun.from("coast", "побережье", "[kəust]", "coasts").isDefined)
  }

  it should "create Phrase successfully" in {
    assert(Phrase.from("How are you?", "Как дела?").isDefined)
  }

  it should "create Preposition successfully" in {
    assert(Preposition.from("above", "над", "[əˈbʌv]").isDefined)
  }

  it should "create Verb successfully" in {
    assert(Verb.from("consist", "состоять", "[kənˈsɪst]",
      "consists", "consisted", "consisting").isDefined)
  }

  "Same nouns" should "be equal" in {
    val nounOpt = Noun.from("coast", "побережье", "[kəust]", "coasts")
    val duplicateOpt = Noun.from("coast", "побережье", "[kəust]", "coasts")
    for {
      noun <- nounOpt
      duplicate <- duplicateOpt
    } yield assert(noun.equals(duplicate))
  }

  "Different adjectives" should "be not equal" in {
    val clumsyOpt = Adjective.from("clumsy", "топорный", "[clumzi]",
      "clumsier", "clumsiest")
    val deceivingOpt =
      Adjective.from("big", "большой", "[bɪɡ]]", "bigger", "biggest")
    for {
      clumsy <- clumsyOpt
      deceiving <- deceivingOpt
    } yield assert(!clumsy.equals(deceiving))
  }

  it should "sort parts of speech by value" in {
    val words = List(
      Adjective.from("high", "высокий", "[hai]", "higher", "highest"),
      Noun.from("coast", "побережье", "[kəust]", "coasts"),
      Phrase.from("How are you?", "Как дела?"),
      Preposition.from("above", "над", "[əˈbʌv]"),
      Verb.from("consist", "состоять", "[kənˈsɪst]",
        "consists", "consisted", "consisting"),
      Adjective.from("clumsy", "топорный", "[clumzi]",
        "clumsier", "clumsiest"),
      Adjective.from("big", "большой", "[bɪɡ]]", "bigger", "biggest")
    )
    val values: List[PartOfSpeech] = for {
      word <- words
      value <- word
    } yield value
    assert(values.sorted.map(_.getValue) == List("above", "big", "clumsy", "coast", "consist", "high", "How are you?"))
  }

  "Preposition" should "be init successfully with multi-translation" in {
    val preposition = Preposition.from("between", "между, среди", "[bɪˈtwi:n]")
    assert(preposition.isDefined)
  }

  "Noun" should "be init successfully with empty plural" in {
    val glasses = Noun.from("glasses", "очки", "[ɡlɑ:siz]", "")
    assert(glasses.isDefined)
  }

  "Noun" should "be init successfully with spaced plural" in {
    val glasses = Noun.from("glasses", "очки", "[ɡlɑ:siz]", "     ")
    assert(glasses.isDefined)
  }
}
