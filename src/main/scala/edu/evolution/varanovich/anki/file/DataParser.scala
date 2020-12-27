package edu.evolution.varanovich.anki.file

import edu.evolution.varanovich.anki.model.PartOfSpeech._

object DataParser {
  def adjective(parts: List[String]): Option[Adjective] =
    Adjective.from(parts.head, parts(2), parts(1), "", "")

  def noun(parts: List[String]): Option[Noun] = Noun.from(parts.head, parts(2), parts(1), "")

  def phrase(parts: List[String]): Option[Phrase] = Phrase.from(parts.head, parts(2))

  def preposition(parts: List[String]): Option[Preposition] = Preposition.from(parts.head, parts(2), parts(1))

  def verb(parts: List[String]): Option[Verb] = Verb.from(parts.head, parts(6), parts(1), parts(5), parts(4), parts(2))
}