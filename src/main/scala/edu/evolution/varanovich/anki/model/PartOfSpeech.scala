package edu.evolution.varanovich.anki.model

import PartOfSpeech._
import edu.evolution.varanovich.anki.utility.WordValidator._

sealed trait PartOfSpeech extends Ordered[PartOfSpeech] {
  def getValue: String

  protected def isEqual(self: PartOfSpeech, that: Any): Boolean = (self, that) match {
    case (self: Adjective, that: Adjective) => self.value == that.value
    case (self: Noun, that: Noun) => self.value == that.value
    case (self: Phrase, that: Phrase) => self.value == that.value
    case (self: Preposition, that: Preposition) => self.value == that.value
    case (self: Verb, that: Verb) => self.value == that.value
    case _ => false
  }

  protected def compareByValue(self: PartOfSpeech, that: PartOfSpeech): Int =
    self.getValue.compareToIgnoreCase(that.getValue)
}

object PartOfSpeech {
  final case class Adjective private(value: String, translation: String, transcription: String, comparative: String,
                                     superlative: String) extends PartOfSpeech {
    override def getValue: String = value
    override def equals(that: Any): Boolean = isEqual(this, that)
    override def compare(that: PartOfSpeech): Int = compareByValue(this, that)
  }
  object Adjective {
    def from(value: String, translation: String, transcription: String, comparative: String,
             superlative: String): Option[Adjective] =
      if (validValue(value) && validTranslation(translation) && validTranscription(transcription) &&
        validOptionalValue(define(comparative)) && validOptionalValue(define(superlative)))
        Some(Adjective(value, translation, transcription, define(comparative), define(superlative))) else None
  }

  final case class Noun private(value: String, translation: String, transcription: String, plural: String)
    extends PartOfSpeech {
    override def getValue: String = value
    override def equals(that: Any): Boolean = isEqual(this, that)
    override def compare(that: PartOfSpeech): Int = compareByValue(this, that)
  }
  object Noun {
    def from(value: String, translation: String, transcription: String, plural: String): Option[Noun] =
      if (validNoun(value) && validTranslation(translation) && validTranscription(transcription) &&
        validOptionalValue(define(plural)))
        Some(Noun(value, translation, transcription, define(plural))) else None
  }

  final case class Phrase private(value: String, translation: String) extends PartOfSpeech {
    override def getValue: String = value
    override def equals(that: Any): Boolean = isEqual(this, that)
    override def compare(that: PartOfSpeech): Int = compareByValue(this, that)
  }
  object Phrase {
    def from(value: String, translation: String): Option[Phrase] =
      if (validPhrase(value) && validPhrase(translation)) Some(Phrase(value, translation)) else None
  }

  final case class Preposition private(value: String, translation: String, transcription: String) extends PartOfSpeech {
    override def getValue: String = value
    override def equals(that: Any): Boolean = isEqual(this, that)
    override def compare(that: PartOfSpeech): Int = compareByValue(this, that)
  }
  object Preposition {
    def from(value: String, translation: String, transcription: String): Option[Preposition] =
      if (validValue(value) && validTranslation(translation) && validTranscription(transcription))
        Some(Preposition(value, translation, transcription)) else None
  }

  final case class Verb private(value: String, translation: String, transcription: String, thirdPerson: String,
                                presentParticiple: String, pastParticiple: String) extends PartOfSpeech {
    override def getValue: String = value
    override def equals(that: Any): Boolean = isEqual(this, that)
    override def compare(that: PartOfSpeech): Int = compareByValue(this, that)
  }
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