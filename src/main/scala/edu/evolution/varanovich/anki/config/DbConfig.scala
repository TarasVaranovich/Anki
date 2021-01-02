package edu.evolution.varanovich.anki.config

import pureconfig.generic.auto.exportReader
import pureconfig.{ConfigReader, ConfigSource, Exported}

case class DbConfig(dbDriver: String,
                    dbUrl: String,
                    dbUser: String,
                    dbPassword: String,
                    transactorPoolSize: Int)
object DbConfig {
  def load: DbConfig = {
    implicit val configReader: Exported[ConfigReader[DbConfig]] = exportReader[DbConfig]
    ConfigSource.default.loadOrThrow[DbConfig]
  }
}
