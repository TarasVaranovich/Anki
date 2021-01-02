package edu.evolution.varanovich.anki.config

import pureconfig.generic.auto.exportReader
import pureconfig.{ConfigReader, ConfigSource, Exported}

case class VocabularyConfig(maxEngWordLength: Int,
                            maxRusWordLength: Int,
                            maxPhraseLength: Int,
                            availablePartsOfSpeechCount: Int)
object VocabularyConfig {
//  val MaxEngWordLength: Int = 45
//  val MaxRusWordLength: Int = 55
//  val MaxPhraseLength: Int = 2000
//  val AvailablePartsOfSpeechCount: Int = 5
  def load: VocabularyConfig = {
    implicit val configReader: Exported[ConfigReader[VocabularyConfig]] = exportReader[VocabularyConfig]
    ConfigSource.default.loadOrThrow[VocabularyConfig]
  }
}
