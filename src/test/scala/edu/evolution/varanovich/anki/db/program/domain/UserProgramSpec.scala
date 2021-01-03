package edu.evolution.varanovich.anki.db.program.domain

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.catsSyntaxApply
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.UserProgram._
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram._
import edu.evolution.varanovich.anki.utility.CryptoUtility.encryptSHA256
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager.runTransaction

class UserProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  private implicit val transactor = DbManager.transactorInstance

  "should successfully perform 'CRUD' operations" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      insertResult <- runTransaction(createUserTable *> createUser(user))
      userModified <- IO.fromOption(userModifiedOpt)(throw new Exception("Modified user not created"))
      updateResult <- runTransaction(updateUser(userModified))
      resultOpt <- runTransaction(readUser(user.name))
      passwordOpt <- runTransaction(readPassword(user.name))
      password <- IO.fromOption(passwordOpt)(throw new Exception("Modified user's password not found"))
      result <- IO.fromOption(resultOpt)(throw new Exception("User not found"))
      deleteResult <- runTransaction(deleteUser(user))
      deleted <- runTransaction(readUser(user.name))
      dropTableResult <- runTransaction(dropTable("anki_user"))
      dropTypeResult <- runTransaction(dropType("privileges_enum"))
    } yield {
      assert(insertResult == 1)
      assert(updateResult == 1)
      assert(result.privileges == userModified.privileges)
      assert(password == encryptSHA256(userModified.password))
      assert(deleteResult == 1)
      assert(deleted.isEmpty)
      assert(dropTableResult == 0)
      assert(dropTypeResult == 0)
    }
  }

  "should successfully lock/unlock user" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      _ <- runTransaction(createUserTable *> createUser(user))
      idOpt <- runTransaction(readUserId(user.name))
      id <- IO.fromOption(idOpt)(throw new Exception("Id not found."))
      _ <- runTransaction(lockUser(id))
      locked <- runTransaction(isLockedUser(user))
      _ <- runTransaction(unlockUser(id))
      unlocked <- runTransaction(isLockedUser(user))
      _ <- runTransaction(dropTable("anki_user"))
      _ <- runTransaction(dropType("privileges_enum"))
    } yield {
      assert(locked)
      assert(!unlocked)
    }
  }
}