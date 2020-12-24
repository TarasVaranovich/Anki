package edu.evolution.varanovich.anki

import org.http4s.implicits.http4sLiteralsSyntax

package object client {
  val NotDefined: String = "Not Defined"
  val ErrorMessage: String = "Server communication error."
  val FirstCard: Int = 1
  val Uri = uri"http://localhost:9001"
}
