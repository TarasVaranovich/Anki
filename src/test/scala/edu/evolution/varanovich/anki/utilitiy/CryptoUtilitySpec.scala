package edu.evolution.varanovich.anki.utilitiy

import edu.evolution.varanovich.anki.utility.CryptoUtility.encryptSHA256
import org.scalatest.freespec.AnyFreeSpec

class CryptoUtilitySpec extends AnyFreeSpec {
  "should identically encrypt the same value" in {
    val password: String = "password"
    val passwordEncrypted: String = encryptSHA256(password)
    val passwordEncryptedSecond: String = encryptSHA256(password)
    assert(passwordEncrypted == passwordEncryptedSecond)
  }
}
