package edu.evolution.varanovich.anki.file

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class DataReaderSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should read file with adjective" in {
    val path: String = "data/english_2/adjective-Table-1.csv"
    DataReader.readFromFile(path, DataParser.adjective).map(adjectives => assert(adjectives.size == 39))
  }

  "should read file with nouns" in {
    val path: String = "data/english_1/noun-Table-1.csv"
    DataReader.readFromFile(path, DataParser.noun).map(nouns => assert(nouns.size == 27))
  }

  "should read file with phrases" in {
    val path: String = "data/english_1/phrase-Table-1.csv"
    DataReader.readFromFile(path, DataParser.phrase).map(phrases => assert(phrases.size == 7))
  }

  "should read file with prepositions" in {
    val path: String = "data/english_1/preposition-Table-1.csv"
    DataReader.readFromFile(path, DataParser.preposition).map(prepositions => assert(prepositions.size == 6))
  }

  "should read file with verbs" in {
    val path: String = "data/english_1/verb-Table-1.csv"
    DataReader.readFromFile(path, DataParser.verb).map(verbs => assert(verbs.size == 17))
  }

  "should read all files with adjectives" in {
    DataReader.all(FileAliases.Adjective, DataParser.adjective).map(adjectives => assert(adjectives.size == 337))
  }

  "should read all files with nouns" in {
    DataReader.all(FileAliases.Noun, DataParser.noun).map(nouns => assert(nouns.size == 412))
  }

  "should read all files with phrases" in {
    DataReader.all(FileAliases.Phrase, DataParser.phrase).map(phrases => assert(phrases.size == 68))
  }

  "should read all files with prepositions" in {
    DataReader.all(FileAliases.Preposition, DataParser.preposition)
      .map(prepositions => assert(prepositions.size == 124))
  }

  "should read all files with verbs" in {
    DataReader.all(FileAliases.Verb, DataParser.verb).map(verbs => assert(verbs.size == 362))
  }
}