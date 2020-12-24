package edu.evolution.varanovich.anki.model

import edu.evolution.varanovich.anki.utility.WordValidator._

/**
 * Class represents part of speech.
 * Each part of speech is defined by it's value and type cause
 * equal part's of speech can have synonym translation.
 *
 * Parts of speech sorted relatively each other by value case insensitive according
 * to common grammatical words rules.
 */
abstract class PartOfSpeech(value: String) extends Ordered[PartOfSpeech] {
  def getValue: String = this.value

  override def compare(that: PartOfSpeech): Int = this.getValue.compare(that.getValue)

  override def equals(that: Any): Boolean = that match {
    case partOfSpeech: PartOfSpeech => partOfSpeech.getValue == this.getValue
    case _ => false
  }
}

object PartOfSpeech {
  final case class Adjective private(value: String, translation: String, transcription: String, comparative: String,
                                     superlative: String) extends PartOfSpeech(value)
  object Adjective {
    def from(value: String, translation: String, transcription: String, comparative: String,
             superlative: String): Option[Adjective] =
      if (validValue(value) && validTranslation(translation) && validTranscription(transcription) &&
        validOptionalValue(define(comparative)) && validOptionalValue(define(superlative)))
        Some(Adjective(value, translation, transcription, define(comparative), define(superlative))) else None
  }

  final case class Noun private(value: String, translation: String, transcription: String, plural: String)
    extends PartOfSpeech(value)
  object Noun {
    def from(value: String, translation: String, transcription: String, plural: String): Option[Noun] =
      if (validNoun(value) && validTranslation(translation) && validTranscription(transcription) &&
        validOptionalValue(define(plural)))
        Some(Noun(value, translation, transcription, define(plural))) else None
  }

  final case class Phrase private(value: String, translation: String) extends PartOfSpeech(value)
  object Phrase {
    def from(value: String, translation: String): Option[Phrase] =
      if (validPhrase(value) && validPhrase(translation)) Some(Phrase(value, translation)) else None
  }

  final case class Preposition private(value: String, translation: String, transcription: String)
    extends PartOfSpeech(value)
  object Preposition {
    def from(value: String, translation: String, transcription: String): Option[Preposition] =
      if (validValue(value) && validTranslation(translation) && validTranscription(transcription))
        Some(Preposition(value, translation, transcription)) else None
  }

  final case class Verb private(value: String, translation: String, transcription: String, thirdPerson: String,
                                presentParticiple: String, pastParticiple: String) extends PartOfSpeech(value)
  object Verb {
    def from(value: String, translation: String, transcription: String, thirdPerson: String,
             presentParticiple: String, pastParticiple: String): Option[Verb] =
      if (validValue(value) && validTranslation(translation) && validTranscription(transcription) &&
        validOptionalValue(define(thirdPerson)) && validOptionalValue(define(presentParticiple)) &&
        validOptionalValue(define(pastParticiple)))
        Some(Verb(value, translation, transcription,
          define(thirdPerson), define(presentParticiple), define(pastParticiple))) else None
  }

  private val define: String => String = (value: String) => if (value.isEmpty || value.isBlank) NotDefined else value
}