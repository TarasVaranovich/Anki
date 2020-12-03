package edu.evolution.varanovich.anki.utility

import java.security.MessageDigest

object CryptoUtility {
  val encryptSHA256: String => String = (value: String) =>
    MessageDigest.getInstance("SHA-256")
      .digest(value.getBytes("UTF-8"))
      .mkString("Array(", ", ", ")")
}
