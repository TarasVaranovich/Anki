package edu.evolution.varanovich.anki.utility

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64

import edu.evolution.varanovich.anki.config.ServerConfig

object CryptoUtility {
  private val secureRandom = new SecureRandom()
  private val base64Encoder = Base64.getUrlEncoder()

  def encryptSHA256(value: String): String = MessageDigest
    .getInstance("SHA-256")
    .digest(value.getBytes("UTF-8"))
    .mkString("Array(", ", ", ")")

  def generateToken: String = {
    val randomBytes = new Array[Byte](ServerConfig.load.tokenSize)
    secureRandom.nextBytes(randomBytes)
    base64Encoder.encodeToString(randomBytes)
  }
}
