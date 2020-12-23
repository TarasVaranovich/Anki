package edu.evolution.varanovich.anki.validator

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedIdBinCompat0}
import edu.evolution.varanovich.anki.model.{Card, Deck, NotDefined}
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxDeckDescriptionLength, MaxDeckLength, MinDeckDescriptionLength, MinDeckLength}
import edu.evolution.varanovich.anki.utility.WordValidator
import edu.evolution.varanovich.anki.utility.WordValidator.{validPhrase, validTranscription, validTranslation, validValue}

object DeckValidator {
  case object DeckLengthError extends ValidationError {
    override def message: String = s"Deck should consist from $MinDeckLength .. $MaxDeckLength cards"
  }
  case object DeckCardError extends ValidationError {
    override def message: String = s"Not all deck cards are parts of speech"
  }
  case object DeckDescriptionLengthError extends ValidationError {
    override def message: String =
      s"Deck description should have length $MinDeckDescriptionLength .. $MaxDeckDescriptionLength"
  }
  case object DeckDescriptionPatternError extends ValidationError {
    override def message: String =
      s"""Can start from eng/rus ABC letters,
         |contain punctuation, signs, quotes, hyphens, percent and gaps.
         |Ends with  eng/rus ABC letter, number, ending punctuation and percent""".stripMargin
  }

  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  def validate(cards: List[Card], description: String): AllErrorsOr[Deck] =
    (validateDeckLength(cards).andThen(validateDeckCards),
      validateDeckDescriptionLength(description).andThen(validateDeckDescriptionPattern))
      .mapN((cards, description) => Deck(cards.toSet, description))

  private def validateDeckLength(cards: List[Card]): AllErrorsOr[List[Card]] =
    if ((MinDeckLength <= cards.length) && (cards.length <= MaxDeckLength)) cards.validNec else
      DeckLengthError.invalidNec

  private def validateDeckCards(cards: List[Card]): AllErrorsOr[List[Card]] = {
    val validateCardField: String => Boolean = (field: String) =>
      validPhrase(field) || validTranslation(field) || field.split("/")
        .forall(part => validTranscription(part) || validValue(part) || (part == NotDefined))
    val validCards = cards
      .filter(card => validateCardField(card.question) && validateCardField(card.answer))
    if (validCards.size == cards.size) cards.validNec else DeckCardError.invalidNec
  }

  private def validateDeckDescriptionLength(description: String): AllErrorsOr[String] =
    if ((MinDeckDescriptionLength <= description.length) && (description.length <= MaxDeckDescriptionLength))
      description.validNec else DeckDescriptionLengthError.invalidNec

  private def validateDeckDescriptionPattern(description: String): AllErrorsOr[String] =
    if (WordValidator.validPhrase(description)) description.validNec else DeckDescriptionPatternError.invalidNec
}