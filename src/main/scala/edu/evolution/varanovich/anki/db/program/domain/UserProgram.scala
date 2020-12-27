package edu.evolution.varanovich.anki.db.program.domain

import java.util.UUID

import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Fragment}
import edu.evolution.varanovich.anki.model.User
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxPasswordEncryptedLength, MaxUserNameLength}
import edu.evolution.varanovich.anki.utility.CryptoUtility.encryptSHA256

//TODO: refactor create user, lock/unlock user after introduction userDto with uuid
object UserProgram {
  def createUserTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TYPE privileges_enum AS ENUM ('Admin', 'Member');
         |CREATE TABLE anki_user(
         |id UUID PRIMARY KEY,
         |id_sequential SERIAL UNIQUE,
         |locked BOOLEAN DEFAULT FALSE,
         |name VARCHAR($MaxUserNameLength) UNIQUE NOT NULL,
         |password VARCHAR($MaxPasswordEncryptedLength) NOT NULL,
         |privileges privileges_enum NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  def createUser(user: User): ConnectionIO[Int] = {
    val query: String =
      s"""INSERT INTO anki_user(
         |id,
         |name,
         |password,
         |privileges) VALUES (
         |'${UUID.randomUUID()}',
         |'${user.name}',
         |'${encryptSHA256(user.password)}',
         |'${user.privileges}');""".stripMargin
    Fragment.const(query).update.run
  }

  def readUser(name: String): ConnectionIO[Option[User]] =
    selectByNameFragment("name" :: "password" :: "privileges" :: Nil, name).query[User].option

  def readPassword(name: String): ConnectionIO[Option[String]] =
    selectByNameFragment("password" :: Nil, name).query[String].option

  def readSequentialId(name: String): ConnectionIO[Option[Int]] =
    selectByNameFragment("id_sequential" :: Nil, name).query[Int].option

  def readUserId(name: String): ConnectionIO[Option[String]] =
    selectByNameFragment("id" :: Nil, name).query[String].option

  def isLockedUser(user: User): ConnectionIO[Boolean] =
    selectByNameFragment("locked" :: Nil, user.name).query[Boolean].option.map {
      case Some(locked) => locked
      case None => false
    }

  def lockUser(id: String): ConnectionIO[Int] = {
    val query: String = s"UPDATE anki_user SET locked = TRUE WHERE id = '$id'"
    Fragment.const(query).update.run
  }

  def unlockUser(id: String): ConnectionIO[Int] = {
    val query: String = s"UPDATE anki_user SET locked = FALSE WHERE id = '$id'"
    Fragment.const(query).update.run
  }

  def updateUser(user: User): ConnectionIO[Int] =
    fr"""UPDATE anki_user SET
        |password = ${encryptSHA256(user.password)},
        |privileges = ${user.privileges}
       WHERE name = ${user.name}""".stripMargin.update.run

  def deleteUser(user: User): ConnectionIO[Int] =
    fr"DELETE FROM anki_user WHERE name = ${user.name}".update.run

  private def selectByNameFragment(fields: List[String], name: String): Fragment =
    fr"SELECT" ++ Fragment.const(fields.mkString(",")) ++ fr"FROM anki_user WHERE name = $name"
}
