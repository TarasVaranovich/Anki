package edu.evolution.varanovich.anki.db.program.entity

import doobie.{ConnectionIO, Fragment}
import edu.evolution.varanovich.anki.model.AnswerInfo

object AnswerInfoProgram {
  def createAnswerInfoTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TYPE rate_enum AS ENUM ('Fail', 'Hard', 'Good', 'Easy');
         |CREATE TABLE answer_info(
         |id SERIAL PRIMARY KEY,
         |card_id INT REFERENCES card(id) ON DELETE CASCADE,
         |rate rate_enum NOT NULL,
         |duration_sec SMALLINT NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  def createAnswerInfo(info: AnswerInfo, cardId: Int): ConnectionIO[Int] = {
    val query: String =
      s"""INSERT INTO answer_info(
         |card_id,
         |rate,
         |duration_sec) VALUES (
         |'$cardId',
         |'${info.rate}',
         |'${info.durationSec}');""".stripMargin
    Fragment.const(query).update.run
  }

  def readAnswerInfoList(cardId: Int): ConnectionIO[List[AnswerInfo]] = {
    val query: String = s"SELECT rate, duration_sec FROM answer_info WHERE card_id = '$cardId'"
    Fragment.const(query).query[AnswerInfo].to[List]
  }
}