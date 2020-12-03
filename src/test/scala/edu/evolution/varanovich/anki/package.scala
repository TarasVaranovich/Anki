package edu.evolution.varanovich

import edu.evolution.varanovich.anki.adt.PartOfSpeech._
import edu.evolution.varanovich.anki.adt.{Card, Deck, Privileges, User}

package object anki {
  //ADJECTIVES
  val bigAdjectiveOpt: Option[Adjective] =
    Adjective.from("big", "большой", "[bɪɡ]]", "bigger", "biggest")
  val clumsyAdjectiveOpt: Option[Adjective] =
    Adjective.from("clumsy", "топорный", "[clumzi]",
      "clumsier", "clumsiest")
  val highAdjectiveOpt: Option[Adjective] =
    Adjective.from("high", "высокий", "[hai]", "higher", "highest")
  val highAdjectiveModifiedOpt: Option[Adjective] =
    Adjective.from("high", "высокий, верхний", "[hai]",
      "higher", "highest")

  //NOUNS
  val coastNounOpt: Option[Noun] =
    Noun.from("coast", "побережье", "[kəust]", "coasts")
  val coastNounModifiedOpt: Option[Noun] =
    Noun.from("coast", "побережье, берег", "[kəust]", "coasts")
  val glassesNounOpt: Option[Noun] = Noun.from("glasses", "очки", "[ɡlɑ:siz]", "")
  val glassesNounWithGapsOpt: Option[Noun] =
    Noun.from("glasses", "очки", "[ɡlɑ:siz]", "     ")

  //PHRASES
  val phraseOpt: Option[Phrase] = Phrase.from("How are you?", "Как дела?")
  val phraseModifiedOpt: Option[Phrase] = Phrase.from("How are you?", "Как дела?, Здравствуйте!")

  //PREPOSITIONS
  val abovePrepositionOpt: Option[Preposition] =
    Preposition.from("above", "над", "[əˈbʌv]")
  val abovePrepositionModifiedOpt: Option[Preposition] =
    Preposition.from("above", "над, выше", "[əˈbʌv]")
  val betweenPrepositionOpt: Option[Preposition] =
    Preposition.from("between", "между, среди", "[bɪˈtwi:n]")

  //VERBS
  val consistVerbOpt: Option[Verb] =
    Verb.from("consist", "состоять", "[kənˈsɪst]",
      "consists", "consisted", "consisting")
  val consistVerbModifiedOpt: Option[Verb] =
    Verb.from("consist", "состоять, заключаться в", "[kənˈsɪst]",
      "consists", "consisted", "consisting")

  //USERS
  val userOpt: Option[User] =
    User.from("Donald Trump", "123456", Privileges.Admin)
  val userModifiedOpt: Option[User] =
    User.from("Donald Trump", "789000", Privileges.Member)

  //DECKS
  val deckOpt: Option[Deck] = for {
    adjective <- bigAdjectiveOpt
    noun <- coastNounOpt
    phrase <- phraseOpt
    preposition <- abovePrepositionOpt
    verb <- consistVerbOpt
    deckOpt <- Deck.from(Set(Card.valueOf(adjective), Card.valueOf(noun), Card.valueOf(phrase),
      Card.valueOf(preposition), Card.valueOf(verb)), "deck opt example")
  } yield deckOpt
}