package edu.evolution.varanovich.anki.utility

import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxPhraseLength, MaxRusWordLength}

import scala.util.matching.Regex

object WordValidator {
  /**
   * Value string can contain lower case letters from english ABC only with possible hyphen between
   */
  def validValue(value: String): Boolean =
    matches(value, "^[a-z]+[\\-]?[a-z]+$".r) && (value.length <= MaxEngWordLength)

  /**
   * Value string can contain english letters from english ABC with possible hyphen or gap between
   */
  def validOptionalValue(value: String): Boolean =
    matches(value, "^[A-Za-z]+[\\-\\s]?[A-Za-z]+$".r) && (value.length <= MaxEngWordLength)

  /**
   * Value string can contain letters from english ABC only with possible hyphen
   */
  def validNoun(value: String): Boolean =
    matches(value, "^[A-Za-z]+[\\-]?[A-Za-z]+$".r) && (value.length <= MaxEngWordLength)

  /**
   * Value string can contain letters from russian ABC only with possible hyphens, spaces and commas
   * String should start from letter and end with letter
   */
  def validTranslation(value: String): Boolean =
    matches(value, "^[ЁёА-я]{1}[ЁёА-я,.)(\\-\\s]*[ЁёА-я.)(]*$".r) && (value.length <= MaxRusWordLength)

  /**
   * Value string can start from english or russian ABC letter or number;
   * can contain english or russian ABC letter, number, punctuation signs, quotes, hyphens, percent an gaps;
   * can end with english or russian ABC letter, number, ending punctuation and percent
   */
  def validPhrase(value: String): Boolean =
    matches(value, "^[A-Za-zЁёА-я0-9]{1}[A-Za-zЁёА-я0-9.,:;?!&%)(\"\'\\-\\s]*[A-Za-zЁёА-я0-9.?!%\"\']{1}$".r) &&
      (value.length <= MaxPhraseLength)

  /**
   * Value string can be enclosed in brackets and contain symbols except . , ! ; ? 0-9 % @ "
   */
  def validTranscription(value: String): Boolean =
    matches(value, "^[\\[]{1}[^.,!;?0-9%@\"]*[\\]]{1}$".r) && value.length <= MaxEngWordLength

  private val matches = (value: String, regex: Regex) => value match {
    case regex(_*) => true
    case _ => false
  }
}