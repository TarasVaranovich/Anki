package edu.evolution.varanovich.anki.utility

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64

import edu.evolution.varanovich.anki.api.http.ServerConfig.tokenSize

object CryptoUtility {
  private val secureRandom = new SecureRandom()
  private val base64Encoder = Base64.getUrlEncoder()

  val encryptSHA256: String => String = (value: String) =>
    MessageDigest.getInstance("SHA-256")
      .digest(value.getBytes("UTF-8"))
      .mkString("Array(", ", ", ")")

  val generateToken: String = {
    val randomBytes = new Array[Byte](tokenSize)
    secureRandom.nextBytes(randomBytes)
    base64Encoder.encodeToString(randomBytes)
  }
}
