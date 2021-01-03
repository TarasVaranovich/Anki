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
                      maxAnswerDuration: Short,
                      generatedDeckName: String)
object AnkiConfig {
  def load: AnkiConfig = {
    implicit val configReader: Exported[ConfigReader[AnkiConfig]] = exportReader[AnkiConfig]
    ConfigSource.default.loadOrThrow[AnkiConfig]
  }
}
