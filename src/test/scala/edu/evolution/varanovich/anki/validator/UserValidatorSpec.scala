package edu.evolution.varanovich.anki.validator

import edu.evolution.varanovich.anki.model.User
import edu.evolution.varanovich.anki.validator.UserValidator.AllErrorsOr
import org.scalatest.freespec.AnyFreeSpec

class UserValidatorSpec extends AnyFreeSpec {
  "successfully creates user " in {
    val result: AllErrorsOr[User] = UserValidator.validate("new-user", "(pa-.,:;?!)", "member")
    assert(result.isValid)
  }

  "collects 3 errors due creation of user with invalid data" in {
    val result: AllErrorsOr[User] = UserValidator.validate("u", "p", "Member")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 3)
  }

  "collects 2 errors due creation of user with invalid data" in {
    val result: AllErrorsOr[User] = UserValidator.validate("User %", "p a & //", "admin")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 2)
  }

  "collects 3 errors due creation of user with empty data" in {
    val result: AllErrorsOr[User] = UserValidator.validate("", "", "")
    val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
    assert(errors.size == 3)
  }
}