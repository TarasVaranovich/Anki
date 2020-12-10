package edu.evolution.varanovich.anki.validator

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple3Semigroupal, catsSyntaxValidatedIdBinCompat0}
import edu.evolution.varanovich.anki.adt.{Privileges, User}
import edu.evolution.varanovich.anki.utility.AnkiConfig._
import edu.evolution.varanovich.anki.utility.StringUtility.matches

object UserValidator {
  case object UserNameLengthError extends ValidationError {
    override def message: String = s"User name should have length $MinUserNameLength .. $MaxUserNameLength"
  }
  case object UserNamePatternError extends ValidationError {
    override def message: String = "User name starts from eng/rus ABC letter, can contain hyphens and digits"
  }
  case object PasswordLengthError extends ValidationError {
    override def message: String = s"Password should have length $MinPasswordLength .. $MaxPasswordLength"
  }
  case object PasswordPatternError extends ValidationError {
    override def message: String =
      "Password can contain eng/rus ABC letters, digits, hyphens, punctuation, round brackets"
  }
  case object UserPrivilegesError extends ValidationError {
    override def message: String = "Privileges can represent singular role in lower case"
  }

  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  def validate(name: String, password: String, privileges: String): AllErrorsOr[User] =
    (validateNameLength(name).andThen(validateNamePattern),
      validatePasswordLength(password).andThen(validatePasswordPattern),
      validatePrivileges(privileges))
      .mapN((name, password, privileges) => User(name, password, Privileges.parseUnsafe(privileges)))

  private def validateNameLength(name: String): AllErrorsOr[String] =
    if ((MinUserNameLength <= name.length) && (name.length <= MaxUserNameLength)) name.validNec else
      UserNameLengthError.invalidNec

  private def validateNamePattern(name: String): AllErrorsOr[String] =
    if (matches(name, "^[A-Za-zЁёА-я]{1}[A-Za-zЁёА-я0-9\\-]*$".r)) name.validNec else
      UserNamePatternError.invalidNec

  private def validatePasswordLength(password: String): AllErrorsOr[String] =
    if ((MinPasswordLength <= password.length) && (password.length <= MaxPasswordLength)) password.validNec else
      PasswordLengthError.invalidNec

  private def validatePasswordPattern(password: String): AllErrorsOr[String] =
    if (matches(password, "[A-Za-zЁёА-я0-9.,:;?!)(\\-]*".r)) password.validNec else
      PasswordPatternError.invalidNec

  private def validatePrivileges(privileges: String): AllErrorsOr[String] =
    Privileges.valueOf(privileges) match {
      case Some(_) => privileges.validNec
      case None => UserPrivilegesError.invalidNec
    }
}