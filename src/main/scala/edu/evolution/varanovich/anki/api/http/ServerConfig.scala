package edu.evolution.varanovich.anki.api.http

object ServerConfig {
  val host: String = "localhost"
  val port: Int = 9001
  val cacheExpiration: Int = 45
  val cacheInvalidation: Int = 15
  val tokenSize: Int = 24
  val loginAttempts: Int = 3
}
