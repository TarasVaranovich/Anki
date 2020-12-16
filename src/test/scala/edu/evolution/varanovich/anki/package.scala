package edu.evolution.varanovich

import edu.evolution.varanovich.anki.adt.PartOfSpeech._
import edu.evolution.varanovich.anki.adt.{Card, Deck, Privileges, User}
import edu.evolution.varanovich.anki.domain.DeckBuilder.GeneratedDeckName

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
  val howAreYouPhraseOpt: Option[Phrase] = Phrase.from("How are you?", "Как дела?")
  val howAreYouPhraseModifiedOpt: Option[Phrase] = Phrase.from("How are you?", "Как дела?, Здравствуйте!")
  val windowOpt: Option[Phrase] = Phrase.from("window of vulnerability", "окно уязвимости")

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
      "consists", "consisting", "consisted")
  val discoverVerbOpt: Option[Verb] =
    Verb.from("discover", "обнаруживать", "[dɪsˈkʌvə]",
      "discovers", "discovering", "discovered")

  //USERS
  val userOpt: Option[User] =
    User.from("Donald Trump", "123456", Privileges.Admin)
  val userOptSecond: Option[User] =
    User.from("Joseph Biden", "654321", Privileges.Admin)
  val userModifiedOpt: Option[User] =
    User.from("Donald Trump", "789000", Privileges.Member)

  //DECKS
  val deckOpt: Option[Deck] =
    for {
      adjective <- bigAdjectiveOpt
      noun <- coastNounOpt
      phrase <- howAreYouPhraseOpt
      preposition <- abovePrepositionOpt
      verb <- consistVerbOpt
      deckOpt <- Deck.from(Set(Card.valueOf(adjective), Card.valueOf(noun), Card.valueOf(phrase),
        Card.valueOf(preposition), Card.valueOf(verb)), "deck opt example")
    } yield deckOpt

  val deckOptSecond: Option[Deck] =
    for {
      adjective <- clumsyAdjectiveOpt
      noun <- glassesNounOpt
      phrase <- windowOpt
      preposition <- betweenPrepositionOpt
      verb <- discoverVerbOpt
      deckOpt <- Deck.from(Set(
        Card.valueOf(adjective),
        Card.valueOf(noun),
        Card.valueOf(phrase),
        Card.valueOf(preposition),
        Card.valueOf(verb)
      ), s"$GeneratedDeckName 202-second")
    } yield deckOpt

  val deckOptThird: Option[Deck] = for {
    adjective <- clumsyAdjectiveOpt
    noun <- glassesNounOpt
    phrase <- windowOpt
    preposition <- betweenPrepositionOpt
    verb <- discoverVerbOpt
    adjectiveTwo <- bigAdjectiveOpt
    nounTwo <- coastNounOpt
    phraseTwo <- howAreYouPhraseOpt
    prepositionTwo <- abovePrepositionOpt
    verbTwo <- consistVerbOpt
    deckOpt <- Deck.from(Set(
      Card.valueOf(adjective),
      Card.valueOf(noun),
      Card.valueOf(phrase),
      Card.valueOf(preposition),
      Card.valueOf(verb),
      Card.valueOf(adjectiveTwo),
      Card.valueOf(nounTwo),
      Card.valueOf(phraseTwo),
      Card.valueOf(prepositionTwo),
      Card.valueOf(verbTwo)
    ), s"$GeneratedDeckName 202-third")
  } yield deckOpt

  val cardListOpt: Option[List[Card]] = for {
    adjective <- bigAdjectiveOpt
    noun <- coastNounOpt
    phrase <- howAreYouPhraseOpt
    preposition <- abovePrepositionOpt
    verb <- consistVerbOpt
  } yield List(adjective, noun, phrase, preposition, verb).map(Card.valueOf)
}