package edu.evolution.varanovich.anki.config

import pureconfig.generic.auto.exportReader
import pureconfig.{ConfigReader, ConfigSource, Exported}

case class AnkiConfig(minUserNameLength: Int,
                      maxUserNameLength: Int,
                      minPasswordLength: Int,
                      maxPasswordLength: Int,
                      maxPasswordEncryptedLength: Int,
                      minDeckLength: Int,
                      maxDeckLength: Int,
                      minDeckDescriptionLength: Int,
                      maxDeckDescriptionLength: Int,
                      maxCardFieldLength: Int,
                      maxAnswerDuration: Short)
object AnkiConfig {
//  val MinUserNameLength: Int = 4
//  val MaxUserNameLength: Int = 15
//  val MinPasswordLength: Int = 4
//  val MaxPasswordLength: Int = 128
//  val MaxPasswordEncryptedLength: Int = 1000
//  val MinDeckLength: Int = 5
//  val MaxDeckLength: Int = 40
//  val MinDeckDescriptionLength: Int = 5
//  val MaxDeckDescriptionLength: Int = 100
//  val MaxCardFieldLength: Int = 100
//  val MaxAnswerDuration: Short = 6000
  def load: AnkiConfig = {
    implicit val configReader: Exported[ConfigReader[AnkiConfig]] = exportReader[AnkiConfig]
    ConfigSource.default.loadOrThrow[AnkiConfig]
  }
}
