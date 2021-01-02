package edu.evolution.varanovich.anki.validator

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple3Semigroupal, catsSyntaxValidatedIdBinCompat0}
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.model.{Privileges, User}

object UserValidator {
  private val config = AnkiConfig.load
  case object UserNameLengthError extends ValidationError {
    override def message: String =
      s"User name should have length ${config.minUserNameLength} .. ${config.maxUserNameLength}"
  }
  case object UserNamePatternError extends ValidationError {
    override def message: String = "User name starts from eng/rus ABC letter, can contain hyphens and digits"
  }
  case object PasswordLengthError extends ValidationError {
    override def message: String =
      s"Password should have length ${config.minPasswordLength} .. ${config.maxPasswordLength}"
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
      .mapN((name, password, privileges) => User(name, password, Privileges.withName(privileges)))

  private def validateNameLength(name: String): AllErrorsOr[String] =
    if ((config.minUserNameLength <= name.length) && (name.length <= config.maxUserNameLength)) name.validNec else
      UserNameLengthError.invalidNec

  private def validateNamePattern(name: String): AllErrorsOr[String] =
    if (name.matches("^[A-Za-zЁёА-я]{1}[A-Za-zЁёА-я0-9\\-]*$")) name.validNec else
      UserNamePatternError.invalidNec

  private def validatePasswordLength(password: String): AllErrorsOr[String] =
    if ((config.minPasswordLength <= password.length) && (password.length <= config.maxPasswordLength))
      password.validNec else
      PasswordLengthError.invalidNec

  private def validatePasswordPattern(password: String): AllErrorsOr[String] =
    if (password.matches("[A-Za-zЁёА-я0-9.,:;?!)(\\-]*")) password.validNec else
      PasswordPatternError.invalidNec

  private def validatePrivileges(privileges: String): AllErrorsOr[String] =
    Privileges.withNameOption(privileges) match {
      case Some(_) => privileges.validNec
      case None => UserPrivilegesError.invalidNec
    }
}