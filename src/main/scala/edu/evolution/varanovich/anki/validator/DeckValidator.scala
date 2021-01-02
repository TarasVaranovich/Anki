package edu.evolution.varanovich.anki.validator

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedIdBinCompat0}
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.model.{Card, Deck}
import edu.evolution.varanovich.anki.utility.WordValidator
import edu.evolution.varanovich.anki.utility.WordValidator._

object DeckValidator {
  private val config = AnkiConfig.load
  case object DeckLengthError extends ValidationError {
    override def message: String = s"Deck should consist from ${config.minDeckLength} .. ${config.maxDeckLength} cards"
  }
  case object DeckCardError extends ValidationError {
    override def message: String = s"Not all deck cards are parts of speech"
  }
  case object DeckDescriptionLengthError extends ValidationError {
    override def message: String =
      s"Deck description should have length ${config.minDeckDescriptionLength} .. ${config.maxDeckDescriptionLength}"
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
    if ((config.minDeckLength <= cards.length) && (cards.length <= config.maxDeckLength)) cards.validNec else
      DeckLengthError.invalidNec

  private def validateDeckCards(cards: List[Card]): AllErrorsOr[List[Card]] = {
    val validateCardField: String => Boolean = (field: String) =>
      validPhrase(field) || validTranslation(field) || field.split("/")
        .forall(part => validTranscription(part) || validValue(part) || validOptionalValue(part))
    val validCards = cards
      .filter(card => validateCardField(card.question) && validateCardField(card.answer))
    if (validCards.size == cards.size) cards.validNec else DeckCardError.invalidNec
  }

  private def validateDeckDescriptionLength(description: String): AllErrorsOr[String] =
    if ((config.minDeckDescriptionLength <= description.length) &&
      (description.length <= config.maxDeckDescriptionLength))
      description.validNec else DeckDescriptionLengthError.invalidNec

  private def validateDeckDescriptionPattern(description: String): AllErrorsOr[String] =
    if (WordValidator.validPhrase(description)) description.validNec else DeckDescriptionPatternError.invalidNec
}