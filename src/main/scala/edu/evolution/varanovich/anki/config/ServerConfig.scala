package edu.evolution.varanovich.anki.config

import pureconfig.generic.auto.exportReader
import pureconfig.{ConfigReader, ConfigSource, Exported}

case class ServerConfig(host: String,
                        port: Int,
                        cacheExpiration: Int,
                        cacheInvalidation: Int,
                        tokenSize: Int,
                        loginAttempts: Int)
object ServerConfig {
  def load: ServerConfig = {
    implicit val configReader: Exported[ConfigReader[ServerConfig]] = exportReader[ServerConfig]
    ConfigSource.default.loadOrThrow[ServerConfig]
  }
}
