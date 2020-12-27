package edu.evolution.varanovich.anki.model

import edu.evolution.varanovich.anki.utility.AnkiConfig.MaxAnswerDuration

final case class AnswerInfo private(rate: Rate, durationSec: Short)
object AnswerInfo {
  def from(rate: Rate, durationSec: Short): Option[AnswerInfo] =
    if ((0 < durationSec) && (durationSec <= MaxAnswerDuration)) Some(AnswerInfo(rate, durationSec)) else None
}
