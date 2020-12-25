package edu.evolution.varanovich.anki.db.program.domain

import doobie.ConnectionIO
import doobie.implicits.toSqlInterpolator
import edu.evolution.varanovich.anki.model.AnswerInfo

object AnswerInfoProgram {
  def createAnswerInfoTable: ConnectionIO[Int] =
    fr"""CREATE TYPE rate_enum AS ENUM ('Fail', 'Hard', 'Good', 'Easy');
        |CREATE TABLE answer_info(
        |id SERIAL PRIMARY KEY,
        |card_id INT REFERENCES card(id) ON DELETE CASCADE,
        |rate rate_enum NOT NULL,
        |duration_sec SMALLINT NOT NULL);""".stripMargin
      .update.run


  def createAnswerInfo(info: AnswerInfo, cardId: Int): ConnectionIO[Int] =
    fr"""INSERT INTO answer_info(
        |card_id,
        |rate,
        |duration_sec) VALUES (
        |$cardId,
        |${info.rate},
        |${info.durationSec});""".stripMargin
      .update.run

  def readAnswerInfoList(cardId: Int): ConnectionIO[List[AnswerInfo]] =
    fr"SELECT rate, duration_sec FROM answer_info WHERE card_id = $cardId".query[AnswerInfo].to[List]
}
