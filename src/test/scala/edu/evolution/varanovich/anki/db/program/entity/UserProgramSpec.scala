package edu.evolution.varanovich.anki.db.program.entity

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import doobie.implicits._
import edu.evolution.varanovich.anki._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.domain.ServiceProgram._
import edu.evolution.varanovich.anki.db.program.entity.UserProgram._
import edu.evolution.varanovich.anki.utility.CryptoUtility.encryptSHA256
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class UserProgramSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully perform 'CRUD' operations" in {
    for {
      user <- IO.fromOption(userOpt)(throw new Exception("User not created."))
      insertResult <- DbManager.transactorBlock(createUserTable *> createUser(user))
      userModified <- IO.fromOption(userModifiedOpt)(throw new Exception("Modified user not created"))
      updateResult <- DbManager.transactor.use(updateUser(userModified).transact[IO])
      resultOpt <- DbManager.transactor.use(readUser(user.name).transact[IO])
      passwordOpt <- DbManager.transactor.use(readPassword(user.name).transact[IO])
      password <- IO.fromOption(passwordOpt)(throw new Exception("Modified user's password not found"))
      result <- IO.fromOption(resultOpt)(throw new Exception("User not found"))
      deleteResult <- DbManager.transactor.use(deleteUser(user).transact[IO])
      deleted <- DbManager.transactor.use(readUser(user.name).transact[IO])
      dropTableResult <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      dropTypeResult <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
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
      _ <- DbManager.transactorBlock(createUserTable *> createUser(user))
      idOpt <- DbManager.transactor.use(readUserId(user.name).transact[IO])
      id <- IO.fromOption(idOpt)(throw new Exception("Id not found."))
      _ <- DbManager.transactor.use(lockUser(id).transact[IO])
      locked <- DbManager.transactor.use(isLockedUser(user).transact[IO])
      _ <- DbManager.transactor.use(unlockUser(id).transact[IO])
      unlocked <- DbManager.transactor.use(isLockedUser(user).transact[IO])
      _ <- DbManager.transactor.use(dropTable("anki_user").transact[IO])
      _ <- DbManager.transactor.use(dropType("privileges_enum").transact[IO])
    } yield {
      assert(locked)
      assert(!unlocked)
    }
  }
}
