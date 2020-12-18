package edu.evolution.varanovich.anki.utility

object AnkiConfig {
  val MinUserNameLength: Int = 4
  val MaxUserNameLength: Int = 15
  val MinPasswordLength: Int = 4
  val MaxPasswordLength: Int = 128
  val MaxPasswordEncryptedLength: Int = 1000
  val MinDeckLength: Int = 5
  val MaxDeckLength: Int = 40
  val MinDeckDescriptionLength: Int = 5
  val MaxDeckDescriptionLength: Int = 100
  val MaxCardFieldLength: Int = 100
  val MaxAnswerDuration: Short = 6000
}
