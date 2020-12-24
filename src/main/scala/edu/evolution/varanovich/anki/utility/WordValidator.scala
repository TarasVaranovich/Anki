package edu.evolution.varanovich.anki.utility

import edu.evolution.varanovich.anki.utility.VocabularyConfig.{MaxEngWordLength, MaxPhraseLength, MaxRusWordLength}

object WordValidator {
  /**
   * Value string can contain lower case letters from english ABC only with possible hyphen between
   */
  def validValue(value: String): Boolean =
    value.matches("^[a-z]+[\\-]?[a-z]+$") && (value.length <= MaxEngWordLength)

  /**
   * Value string can contain english letters from english ABC with possible hyphen or gap between
   */
  def validOptionalValue(value: String): Boolean =
    value.isEmpty || value.matches("^[A-Za-z]+[\\-\\s]?[A-Za-z]+$") && (value.length <= MaxEngWordLength)

  /**
   * Value string can contain letters from english ABC only with possible hyphen
   */
  def validNoun(value: String): Boolean =
    value.matches("^[A-Za-z]+[\\-]?[A-Za-z]+$") && (value.length <= MaxEngWordLength)

  /**
   * Value string can contain letters from russian ABC only with possible hyphens, spaces and commas
   * String should start from letter and end with letter
   */
  def validTranslation(value: String): Boolean =
    value.matches("^[ЁёА-я]{1}[ЁёА-я,.)(\\-\\s]*[ЁёА-я.)(]*$") && (value.length <= MaxRusWordLength)

  /**
   * Value string can start from english or russian ABC letter or number;
   * can contain english or russian ABC letter, number, punctuation signs, quotes, hyphens, percent and gaps;
   * can end with english or russian ABC letter, number, ending punctuation and percent
   */
  def validPhrase(value: String): Boolean =
    value.matches("^[A-Za-zЁёА-я0-9]{1}[A-Za-zЁёА-я0-9.,:;?!&%)(\"\'\\-\\s]*[A-Za-zЁёА-я0-9.?!%\"\']{1}$") &&
      (value.length <= MaxPhraseLength)

  /**
   * Value string can be enclosed in brackets and contain symbols except . , ! ; ? 0-9 % @ "
   */
  def validTranscription(value: String): Boolean =
    value.matches("^[\\[]{1}[^.,!;?0-9%@\"]*[\\]]{1}$") && value.length <= MaxEngWordLength
}