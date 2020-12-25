package edu.evolution.varanovich.anki.file

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class DataReaderSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should read file with adjective" in {
    val path: String = "data/english_2/adjective-Table-1.csv"
    for {
      adjectives <- DataReader.readFromFile(path, DataParser.adjective)
    } yield assert(adjectives.size == 39)
  }

  "should read file with nouns" in {
    val path: String = "data/english_1/noun-Table-1.csv"
    for {
      nouns <- DataReader.readFromFile(path, DataParser.noun)
    } yield assert(nouns.size == 27)
  }

  "should read file with phrases" in {
    val path: String = "data/english_1/phrase-Table-1.csv"
    for {
      phrases <- DataReader.readFromFile(path, DataParser.phrase)
    } yield assert(phrases.size == 7)
  }

  "should read file with prepositions" in {
    val path: String = "data/english_1/preposition-Table-1.csv"
    for {
      prepositions <- DataReader.readFromFile(path, DataParser.preposition)
    } yield assert(prepositions.size == 6)
  }

  "should read file with verbs" in {
    val path: String = "data/english_1/verb-Table-1.csv"
    for {
      verbs <- DataReader.readFromFile(path, DataParser.verb)
    } yield assert(verbs.size == 17)
  }

  "should read all files with adjectives" in {
    for {
      adjectives <- DataReader.all(FileAliases.Adjective, DataParser.adjective)
    } yield assert(adjectives.size == 337)
  }

  "should read all files with nouns" in {
    for {
      nouns <- DataReader.all(FileAliases.Noun, DataParser.noun)
    } yield assert(nouns.size == 412)
  }

  "should read all files with phrases" in {
    for {
      phrases <- DataReader.all(FileAliases.Phrase, DataParser.phrase)
    } yield assert(phrases.size == 68)
  }

  "should read all files with prepositions" in {
    for {
      prepositions <- DataReader.all(FileAliases.Preposition, DataParser.preposition)
    } yield assert(prepositions.size == 124)
  }

  "should read all files with verbs" in {
    for {
      verbs <- DataReader.all(FileAliases.Verb, DataParser.verb)
    } yield assert(verbs.size == 362)
  }
}