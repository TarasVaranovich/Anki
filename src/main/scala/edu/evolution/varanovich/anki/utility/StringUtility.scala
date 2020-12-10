package edu.evolution.varanovich.anki.utility

import scala.util.matching.Regex

object StringUtility {
  val matches = (value: String, regex: Regex) => value match {
    case regex(_*) => true
    case _ => false
  }
}
