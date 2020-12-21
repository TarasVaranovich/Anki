package edu.evolution.varanovich.anki.validator

import edu.evolution.varanovich.anki.adt.{Card, Deck}
import edu.evolution.varanovich.anki.cardListOpt
import edu.evolution.varanovich.anki.utility.WordValidator.validTranscription
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

  "collects 2 errors due creation of deck with invalid card" in {
    val cards = Card("1", "") :: cardListOpt.getOrElse(List())
    val result: AllErrorsOr[Deck] = DeckValidator.validate(cards, "")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 2)
  }

  "successfully creates deck with custom part of speech" in {
    val cards = Card("запускать, выстрелить", "fire/[ˈfaɪə]/fires/Not Defined/fired") :: cardListOpt.getOrElse(List())
    val result: AllErrorsOr[Deck] = DeckValidator.validate(cards, "This is new deck from 6 cards.")
    assert(result.isValid)
  }

  "collects 1 error due creation of deck with invalid card answer" in {
    val cards = Card("запускать, выстрелить", "fire/[ˈfaɪə]/запускать/firing/fired") :: cardListOpt.getOrElse(List())
    val result: AllErrorsOr[Deck] = DeckValidator.validate(cards, "Description")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 1)
  }

  "successfully creates deck with synthetic part of speech" in {
    val cards = Card("карта один вопрос один", "card one question one") :: cardListOpt.getOrElse(List())
    val result: AllErrorsOr[Deck] = DeckValidator.validate(cards, "Description")
    assert(result.isValid)
  }
}