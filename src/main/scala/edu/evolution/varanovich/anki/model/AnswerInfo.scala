package edu.evolution.varanovich.anki.model

import edu.evolution.varanovich.anki.config.AnkiConfig

final case class AnswerInfo private(rate: Rate, durationSec: Short)
object AnswerInfo {
  private val maxAnswerDuration = AnkiConfig.load.maxAnswerDuration
  def from(rate: Rate, durationSec: Short): Option[AnswerInfo] =
    if ((0 < durationSec) && (durationSec <= maxAnswerDuration)) Some(AnswerInfo(rate, durationSec)) else None
}
