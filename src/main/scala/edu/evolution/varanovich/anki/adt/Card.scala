package edu.evolution.varanovich.anki.adt

import edu.evolution.varanovich.anki.adt.PartOfSpeech.{Adjective, Preposition, Verb}

final case class Card private(question: String, answer: String) extends Ordered[Card] {
  override def equals(that: Any): Boolean = that match {
    case that: Card => question.equalsIgnoreCase(that.question)
    case _ => false
  }

  override def compare(that: Card): Int = this.question.compareToIgnoreCase(that.question)
}
object Card {
  def valueOf(partOfSpeech: PartOfSpeech): Card = partOfSpeech match {
    case Adjective(value, translation, transcription, comparative, superlative) =>
      Card(translation, s"$value/$transcription/$comparative/$superlative")
    case PartOfSpeech.Noun(value, translation, transcription, plural) =>
      Card(translation, s"$value/$transcription/$plural")
    case PartOfSpeech.Phrase(value, translation) =>
      Card(translation, s"$value")
    case Preposition(value, translation, transcription) =>
      Card(translation, s"$value/$transcription")
    case Verb(value, translation, transcription, thirdPerson, presentParticiple, pastParticiple) =>
      Card(translation, s"$value/$transcription/$thirdPerson/$presentParticiple/$pastParticiple")
  }
}