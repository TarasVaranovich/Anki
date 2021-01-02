package edu.evolution.varanovich.anki.config

import org.scalatest.freespec.AnyFreeSpec

import scala.util.Try

class ConfigSpec extends AnyFreeSpec {
  "should load server config" in {
    assert(Try(ServerConfig.load).isSuccess)
  }

  "should load db config" in {
    assert(Try(DbConfig.load).isSuccess)
  }

  "should load vocabulary config" in {
    assert(Try(VocabularyConfig.load).isSuccess)
  }

  "should load anki config" in {
    assert(Try(AnkiConfig.load).isSuccess)
  }
}
