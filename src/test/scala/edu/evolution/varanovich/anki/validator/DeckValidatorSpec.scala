package edu.evolution.varanovich.anki.validator

import edu.evolution.varanovich.anki.adt.Deck
import edu.evolution.varanovich.anki.cardListOpt
import edu.evolution.varanovich.anki.validator.UserValidator.AllErrorsOr
import org.scalatest.freespec.AnyFreeSpec

class DeckValidatorSpec extends AnyFreeSpec {
  "successfully creates deck " in {
    val cards = cardListOpt.getOrElse(List())
    val result: AllErrorsOr[Deck] = DeckValidator.validate(cards, "This is new deck from 5 cards.")
    assert(result.isValid)
  }

  "collects 2 errors due creation of deck with invalid data" in {
    val result: AllErrorsOr[Deck] = DeckValidator.validate(List(), "~@1234567890")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 2)
  }

  "collects 2 errors due creation of deck with empty data" in {
    val result: AllErrorsOr[Deck] = DeckValidator.validate(List(), "")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 2)
  }
}