package edu.evolution.varanovich.anki.db.program.domain

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment}
import edu.evolution.varanovich.anki.config.AnkiConfig
import edu.evolution.varanovich.anki.model.Deck

object DeckProgram {
  private val maxDeckDescriptionLength = AnkiConfig.load.maxDeckDescriptionLength
  def createDeckTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TABLE deck(
         |id SERIAL PRIMARY KEY,
         |user_id INT REFERENCES anki_user(id_sequential) ON DELETE CASCADE,
         |description VARCHAR($maxDeckDescriptionLength) NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  def createDeck(deck: Deck, userId: Int): ConnectionIO[Int] =
    fr"INSERT INTO deck(description, user_id) VALUES (${deck.description}, $userId)".update.run

  def readDeckIdByDescriptionAndUserName(description: String, name: String): ConnectionIO[Option[Int]] =
    selectJoinUserFragment(name, fr"AND deck.description = $description").query[Int].option

  def readLastDeckInfoByPatternAndUserName(pattern: String, name: String): ConnectionIO[Option[(Int, String)]] =
    (selectJoinUserFragment(name,
      fr"AND deck.description LIKE ${pattern + "%"}",
      fr", deck.description") ++
      orderLimitFragment(true, 1)).query[(Int, String)].option

  def readEarliestFreshDeckInfo(name: String): ConnectionIO[Option[(Int, String)]] =
    (selectJoinUserFragment(name,
      fr"AND answer_info.id IS NULL",
      fr", deck.description",
      fr"DISTINCT",
      fr"INNER JOIN card ON deck.id = card.deck_id LEFT OUTER JOIN answer_info ON card.id = answer_info.card_id") ++
      orderLimitFragment(false, 1))
      .query[(Int, String)].option

  def readDeckIdListByUserName(name: String): ConnectionIO[List[Int]] = selectJoinUserFragment(name).query[Int].to[List]

  def deleteDeck(deck: Deck): ConnectionIO[Int] = fr"DELETE FROM deck WHERE description = ${deck.description}".update.run

  private def selectJoinUserFragment(userName: String,
                                     clauseFragment: Fragment = Fragment.empty,
                                     fieldFragment: Fragment = Fragment.empty,
                                     preFilterFragment: Fragment = Fragment.empty,
                                     joinFragment: Fragment = Fragment.empty): Fragment =
    fr"SELECT" ++ preFilterFragment ++ fr"deck.id" ++ fieldFragment ++
      fr"FROM deck INNER JOIN anki_user ON deck.user_id = anki_user.id_sequential" ++ joinFragment ++
      fr"WHERE anki_user.name = $userName" ++ clauseFragment

  private def orderLimitFragment(isDesc: Boolean, limit: Int): Fragment =
    fr"ORDER BY deck.id" ++ (if (isDesc) fr"DESC" else fr"ASC") ++ fr"LIMIT $limit"
}