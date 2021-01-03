package edu.evolution.varanovich.anki.domain

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats.effect.{IO, Resource}
import doobie.Transactor
import edu.evolution.varanovich.anki.config.{AnkiConfig, VocabularyConfig}
import edu.evolution.varanovich.anki.db.DbManager.runTransaction
import edu.evolution.varanovich.anki.db.program.entity.AdjectiveProgram.readAdjectiveById
import edu.evolution.varanovich.anki.db.program.entity.NounProgram.readNounById
import edu.evolution.varanovich.anki.db.program.entity.PhraseProgram.readPhraseById
import edu.evolution.varanovich.anki.db.program.entity.PrepositionProgram.readPrepositionById
import edu.evolution.varanovich.anki.db.program.entity.VerbProgram.readVerbById
import edu.evolution.varanovich.anki.db.program.service.ServiceProgram.{readMaxId, readRowsCount}
import edu.evolution.varanovich.anki.domain.Alias._
import edu.evolution.varanovich.anki.model.{Card, Deck, PartOfSpeech}

import scala.util.Random

final case class DeckBuilder(config: AnkiConfig)(implicit transactor: Resource[IO, Transactor[IO]]) {
  def randomDeck(size: Int): IO[Option[Deck]] = {
    for {
      partition <- partitionSelection(size)
      adjectiveMaxIdOpt <- runTransaction(readMaxId(Alias.Adjective.name))
      adjectiveMaxId <- IO(adjectiveMaxIdOpt.getOrElse(0))
      adjectiveSet <- getRandomPartOfSpeechSet(partition.averageSize, adjectiveMaxId, Adjective)
      nounMaxIdOpt <- runTransaction(readMaxId(Alias.Noun.name))
      nounMaxId <- IO(nounMaxIdOpt.getOrElse(0))
      nounSet <- getRandomPartOfSpeechSet(partition.averageSize, nounMaxId, Noun)
      phraseMaxIdOpt <- runTransaction(readMaxId(Alias.Phrase.name))
      phraseMaxId <- IO(phraseMaxIdOpt.getOrElse(0))
      phraseSet <- getRandomPartOfSpeechSet(partition.averageSize, phraseMaxId, Phrase)
      prepositionMaxIdOpt <- runTransaction(readMaxId(Alias.Preposition.name))
      prepositionMaxId <- IO(prepositionMaxIdOpt.getOrElse(0))
      prepositionSet <- getRandomPartOfSpeechSet(partition.averageSize, prepositionMaxId, Preposition)
      verbMaxIdOpt <- runTransaction(readMaxId(Alias.Verb.name))
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
      Deck.from(summary, s"${config.generatedDeckName} ${LocalDateTime.now().format(formatter)}")
    }
  }

  private def partitionSelection(cardCount: Int): IO[Partition] = {
    for {
      adjectiveCountOpt <- runTransaction(readRowsCount(Adjective.name))
      adjectiveCount <- IO(adjectiveCountOpt.getOrElse(0))
      nounCountOpt <- runTransaction(readRowsCount(Noun.name))
      nounCount <- IO(nounCountOpt.getOrElse(0))
      phraseCountOpt <- runTransaction(readRowsCount(Phrase.name))
      phraseCount <- IO(phraseCountOpt.getOrElse(0))
      prepositionCountOpt <- runTransaction(readRowsCount(Preposition.name))
      prepositionCount <- IO(prepositionCountOpt.getOrElse(0))
      verbCountOpt <- runTransaction(readRowsCount(Verb.name))
      verbCount <- IO(verbCountOpt.getOrElse(0))
    } yield {
      val counts = adjectiveCount :: nounCount :: phraseCount :: prepositionCount :: verbCount :: Nil
      calculatePartition(counts, verbCount, cardCount)
    }
  }

  private def calculatePartition(counts: List[Int], supporterCount: Int, cardCount: Int): Partition = {
    val availablePartsOfSpeechCount = VocabularyConfig.load.availablePartsOfSpeechCount
    val summary = counts.sum
    if ((summary >= cardCount) && (cardCount >= availablePartsOfSpeechCount)) {
      val averagePartitionSize = Math.floor(cardCount / availablePartsOfSpeechCount).toInt
      val remain: Int = cardCount - averagePartitionSize * availablePartsOfSpeechCount
      if (counts.forall(count => count >= averagePartitionSize) &&
        (supporterCount >= (averagePartitionSize + availablePartsOfSpeechCount))) {
        Partition(averagePartitionSize, averagePartitionSize + remain)
      } else Partition.empty
    } else Partition.empty
  }

  private def getRandomPartOfSpeechSet(count: Int,
                                       rangeLimit: Int,
                                       partOfSpeech: Alias,
                                       partOfSpeechSet: Set[PartOfSpeech] = Set()): IO[Set[PartOfSpeech]] = {
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

  private def resolveQuery(partOfSpeech: Alias, id: Int): IO[Option[PartOfSpeech]] = partOfSpeech match {
    case Adjective => runTransaction(readAdjectiveById(id))
    case Noun => runTransaction(readNounById(id))
    case Phrase => runTransaction(readPhraseById(id))
    case Preposition => runTransaction(readPrepositionById(id))
    case Verb => runTransaction(readVerbById(id))
  }
}