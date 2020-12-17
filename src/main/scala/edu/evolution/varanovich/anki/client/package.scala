package edu.evolution.varanovich.anki

import org.http4s.implicits.http4sLiteralsSyntax

package object client {
  val ErrorMessage: String = "Connection error."
  val FirstCard: Int = 1
  val Uri = uri"http://localhost:9001"
}
