package edu.evolution.varanovich.anki.db.program.service

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment}

object ServiceProgram {
  def readMaxId(name: String): ConnectionIO[Option[Int]] =
    (fr"SELECT MAX(id) from" ++ Fragment.const(name)).query[Int].option

  def readRowsCount(name: String): ConnectionIO[Option[Int]] =
    (fr"SELECT COUNT(*) from" ++ Fragment.const(name)).query[Int].option

  def dropTable(name: String): ConnectionIO[Int] =
    (fr"DROP TABLE IF EXISTS" ++ Fragment.const(name)).update.run

  def dropType(name: String): ConnectionIO[Int] =
    (fr"DROP TYPE IF EXISTS" ++ Fragment.const(name)).update.run
}
