package edu.evolution.varanovich.anki.domain

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats.effect.{ContextShift, IO}
import doobie.implicits._
import edu.evolution.varanovich.anki.db.DbManager
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.readAdjectiveById
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.readNounById
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.readPhraseById
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.readPrepositionById
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.readVerbById
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram.{readMaxId, readRowsCount}
import edu.evolution.varanovich.anki.domain.DeckBuilder.Alias._
import edu.evolution.varanovich.anki.model.{Card, Deck, PartOfSpeech}
import edu.evolution.varanovich.anki.utility.VocabularyConfig.AvailablePartsOfSpeechCount

import scala.util.Random

object DeckBuilder {
  val GeneratedDeckName: String = "Automatically generated deck from"
  sealed trait Alias {
    def name: String
  }
  object Alias {
    case object Adjective extends Alias {
      override def name: String = "adjective"
    }
    case object Noun extends Alias {
      override def name: String = "noun"
    }
    case object Phrase extends Alias {
      override def name: String = "phrase"
    }
    case object Preposition extends Alias {
      override def name: String = "preposition"
    }
    case object Verb extends Alias {
      override def name: String = "verb"
    }
  }

  final case class Partition(averageSize: Int, extendedSize: Int)
  object Partition {
    def empty: Partition = Partition(0, 0)
  }

  def randomDeck(size: Int)(implicit contextShift: ContextShift[IO]): IO[Option[Deck]] = {
    for {
      partition <- partitionSelection(size)
      adjectiveMaxIdOpt <- DbManager.transactor.use(readMaxId(Alias.Adjective.name).transact[IO])
      adjectiveMaxId <- IO(adjectiveMaxIdOpt.getOrElse(0))
      adjectiveSet <- getRandomPartOfSpeechSet(partition.averageSize, adjectiveMaxId, Adjective)
      nounMaxIdOpt <- DbManager.transactor.use(readMaxId(Alias.Noun.name).transact[IO])
      nounMaxId <- IO(nounMaxIdOpt.getOrElse(0))
      nounSet <- getRandomPartOfSpeechSet(partition.averageSize, nounMaxId, Noun)
      phraseMaxIdOpt <- DbManager.transactor.use(readMaxId(Alias.Phrase.name).transact[IO])
      phraseMaxId <- IO(phraseMaxIdOpt.getOrElse(0))
      phraseSet <- getRandomPartOfSpeechSet(partition.averageSize, phraseMaxId, Phrase)
      prepositionMaxIdOpt <- DbManager.transactor.use(readMaxId(Alias.Preposition.name).transact[IO])
      prepositionMaxId <- IO(prepositionMaxIdOpt.getOrElse(0))
      prepositionSet <- getRandomPartOfSpeechSet(partition.averageSize, prepositionMaxId, Preposition)
      verbMaxIdOpt <- DbManager.transactor.use(readMaxId(Alias.Verb.name).transact[IO])
      verbMaxId <- IO(verbMaxIdOpt.getOrElse(0))
      verbSet <- getRandomPartOfSpeechSet(partition.extendedSize, verbMaxId, Verb)
    } yield {
      val adjectiveCards = adjectiveSet.map(Card.valueOf)
      val nounCards = nounSet.map(Card.valueOf)
      val phraseCards = phraseSet.map(Card.valueOf)
      val prepositionCards = prepositionSet.map(Card.valueOf)
      val verbCards = verbSet.map(Card.valueOf)
      val summary = adjectiveCards ++ nounCards ++ phraseCards ++ prepositionCards ++ verbCards
      val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      Deck.from(summary, s"$GeneratedDeckName ${LocalDateTime.now().format(formatter)}")
    }
  }

  private def partitionSelection(cardCount: Int)(implicit contextShift: ContextShift[IO]): IO[Partition] = {
    for {
      adjectiveCountOpt <- DbManager.transactor.use(readRowsCount(Adjective.name).transact[IO])
      adjectiveCount <- IO(adjectiveCountOpt.getOrElse(0))
      nounCountOpt <- DbManager.transactor.use(readRowsCount(Noun.name).transact[IO])
      nounCount <- IO(nounCountOpt.getOrElse(0))
      phraseCountOpt <- DbManager.transactor.use(readRowsCount(Phrase.name).transact[IO])
      phraseCount <- IO(phraseCountOpt.getOrElse(0))
      prepositionCountOpt <- DbManager.transactor.use(readRowsCount(Preposition.name).transact[IO])
      prepositionCount <- IO(prepositionCountOpt.getOrElse(0))
      verbCountOpt <- DbManager.transactor.use(readRowsCount(Verb.name).transact[IO])
      verbCount <- IO(verbCountOpt.getOrElse(0))
    } yield {
      val counts = adjectiveCount :: nounCount :: phraseCount :: prepositionCount :: verbCount :: Nil
      calculatePartition(counts, verbCount, cardCount)
    }
  }

  private def calculatePartition(counts: List[Int], supporterCount: Int, cardCount: Int): Partition = {
    val summary = counts.sum
    if ((summary >= cardCount) && (cardCount >= AvailablePartsOfSpeechCount)) {
      val averagePartitionSize = Math.floor(cardCount / AvailablePartsOfSpeechCount).toInt
      val remain: Int = cardCount - averagePartitionSize * AvailablePartsOfSpeechCount
      if (counts.forall(count => count >= averagePartitionSize) &&
        (supporterCount >= (averagePartitionSize + AvailablePartsOfSpeechCount))) {
        Partition(averagePartitionSize, averagePartitionSize + remain)
      } else Partition.empty
    } else Partition.empty
  }

  private def getRandomPartOfSpeechSet(count: Int,
                                       rangeLimit: Int,
                                       partOfSpeech: Alias,
                                       partOfSpeechSet: Set[PartOfSpeech] = Set())(implicit contextShift: ContextShift[IO]):
  IO[Set[PartOfSpeech]] = {
    if (partOfSpeechSet.size < count) {
      val id = Random.between(1, rangeLimit)
      for {
        resultOption <- resolveQuery(partOfSpeech, id)
        partOfSpeechSet <- resultOption match {
          case Some(value) => getRandomPartOfSpeechSet(count, rangeLimit, partOfSpeech, (partOfSpeechSet ++ Set(value)))
          case None => getRandomPartOfSpeechSet(count, rangeLimit, partOfSpeech, partOfSpeechSet)
        }
      } yield partOfSpeechSet
    } else IO(partOfSpeechSet)
  }

  private def resolveQuery(partOfSpeech: Alias, id: Int)(implicit contextShift: ContextShift[IO]):
  IO[Option[PartOfSpeech]] = partOfSpeech match {
    case Adjective => DbManager.transactor.use(readAdjectiveById(id).transact[IO])
    case Noun => DbManager.transactor.use(readNounById(id).transact[IO])
    case Phrase => DbManager.transactor.use(readPhraseById(id).transact[IO])
    case Preposition => DbManager.transactor.use(readPrepositionById(id).transact[IO])
    case Verb => DbManager.transactor.use(readVerbById(id).transact[IO])
  }
}