package edu.evolution.varanovich.anki.db.program.entity

import java.util.UUID

import doobie.ConnectionIO
import doobie.util.fragment.Fragment
import edu.evolution.varanovich.anki.adt.User
import edu.evolution.varanovich.anki.utility.AnkiConfig.{MaxPasswordEncryptedLength, MaxUserNameLength}
import edu.evolution.varanovich.anki.utility.CryptoUtility.encryptSHA256

object UserProgram {
  val createUserTable: ConnectionIO[Int] = {
    val query: String =
      s"""CREATE TYPE privileges_enum AS ENUM ('Admin', 'Member');
         |CREATE TABLE anki_user(
         |id UUID PRIMARY KEY,
         |id_sequential SERIAL UNIQUE,
         |name VARCHAR($MaxUserNameLength) UNIQUE NOT NULL,
         |password VARCHAR($MaxPasswordEncryptedLength) NOT NULL,
         |privileges privileges_enum NOT NULL);""".stripMargin
    Fragment.const(query).update.run
  }

  val createUser: User => ConnectionIO[Int] = (user: User) => {
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

  val readUser: String => ConnectionIO[Option[User]] = (value: String) => {
    val query: String =
      s"""SELECT
         |name,
         |password,
         |privileges
         |FROM anki_user WHERE name = '$value'""".stripMargin
    Fragment.const(query).query[User].option
  }

  val readPassword: String => ConnectionIO[Option[String]] = (value: String) => {
    val query: String = s"SELECT password FROM anki_user WHERE name = '$value'"
    Fragment.const(query).query[String].option
  }

  val readSequentialId: String => ConnectionIO[Option[Int]] = (value: String) => {
    val query: String = s"SELECT id_sequential FROM anki_user WHERE name = '$value'".stripMargin
    Fragment.const(query).query[Int].option
  }

  val updateUser: User => ConnectionIO[Int] = (user: User) => {
    val query: String =
      s"""UPDATE anki_user SET
         |password = '${encryptSHA256(user.password)}',
         |privileges = '${user.privileges}'
       WHERE name = '${user.name}'""".stripMargin
    Fragment.const(query).update.run
  }

  val deleteUser: User => ConnectionIO[Int] = (user: User) => {
    val query: String = s"DELETE FROM anki_user WHERE name = '${user.name}'"
    Fragment.const(query).update.run
  }
}
