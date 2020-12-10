package edu.evolution.varanovich.anki.validator

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedIdBinCompat0}
import edu.evolution.varanovich.anki.adt.{Card, Deck}
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxDeckDescriptionLength, MaxDeckLength, MinDeckDescriptionLength, MinDeckLength}
import edu.evolution.varanovich.anki.utility.WordValidator

object DeckValidator {
  case object DeckLengthError extends ValidationError {
    override def message: String = s"Deck should consist from $MinDeckLength .. $MaxDeckLength"
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
    (validateDeckLength(cards),
      validateDeckDescriptionLength(description).andThen(validateDeckDescriptionPattern))
      .mapN((cards, description) => Deck(cards.toSet, description))

  private def validateDeckLength(cards: List[Card]): AllErrorsOr[List[Card]] =
    if ((MinDeckLength <= cards.length) && (cards.length <= MaxDeckLength)) cards.validNec else
      DeckLengthError.invalidNec

  private def validateDeckDescriptionLength(description: String): AllErrorsOr[String] =
    if ((MinDeckDescriptionLength <= description.length) && (description.length <= MaxDeckDescriptionLength))
      description.validNec else DeckDescriptionLengthError.invalidNec

  private def validateDeckDescriptionPattern(description: String): AllErrorsOr[String] =
    if (WordValidator.validPhrase(description)) description.validNec else DeckDescriptionPatternError.invalidNec
}