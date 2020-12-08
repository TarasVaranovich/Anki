package edu.evolution.varanovich.anki.file

import edu.evolution.varanovich.anki.adt.PartOfSpeech._

object DataParser {
  val adjective: (List[String] => Option[Adjective]) = (parts: List[String]) =>
    Adjective.from(parts.head, parts(2), parts(1), "", "")

  val noun: (List[String] => Option[Noun]) = (parts: List[String]) =>
    Noun.from(parts.head, parts(2), parts(1), "")

  val phrase: (List[String] => Option[Phrase]) = (parts: List[String]) =>
    Phrase.from(parts.head, parts(2))

  val preposition: (List[String] => Option[Preposition]) = (parts: List[String]) =>
    Preposition.from(parts.head, parts(2), parts(1))

  val verb: (List[String] => Option[Verb]) = (parts: List[String]) =>
    Verb.from(parts.head, parts(6), parts(1), parts(5), parts(4), parts(2))
}
