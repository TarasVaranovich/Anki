package edu.evolution.varanovich.anki.api.http

object AnkiErrorCode {
  val AlreadyExists: Int = -1
  val ServerError: Int = -2
  val NotExists: Int = -3
  val WrongPassword = -4
  val Blocked = -5
  val OperationSuccess: Int = 1
}
