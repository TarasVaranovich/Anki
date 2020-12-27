package edu.evolution.varanovich.anki.utilitiy

import edu.evolution.varanovich.anki.utility.WordValidator.{validNoun, validOptionalValue, validPhrase, validTranscription, validTranslation, validValue}
import org.scalatest.freespec.AnyFreeSpec

class WordValidatorSpec extends AnyFreeSpec {
  "Valid word" - {
    "is valid" in {
      assert(validValue("word"))
    }

    "is valid with hyphen" in {
      assert(validValue("semi-conductor"))
    }

    "is ot valid with quotes" in {
      assert(!validValue("'word"))
    }

    "is not valid with upper case letters" in {
      assert(!validValue("'woRd"))
    }

    "is not valid with numbers" in {
      assert(!validValue("'word96"))
    }

    "is not valid with cyrillic letters" in {
      assert(!validValue("мир"))
    }

    "is not valid with hyphen position" in {
      assert(!validValue("-semi"))
    }

    "is not valid with gap between" in {
      assert(!validValue("not valid"))
    }
  }

  "Valid optional word" - {
    "is valid solid word" in {
      assert(validOptionalValue("higher"))
    }

    "is valid empty string" in {
      assert(validOptionalValue(""))
    }

    "is valid with hyphen" in {
      assert(validOptionalValue("half-completed"))
    }

    "is valid with gap position and upper case" in {
      assert(validOptionalValue("put outside"))
    }

    "is not valid in case of start from gap" in {
      assert(!validOptionalValue("  highest"))
    }

    "is not valid in case of end on hyphen" in {
      assert(!validOptionalValue("highest-"))
    }
  }

  "Valid noun" - {
    "is valid" in {
      assert(validNoun("Moon"))
    }
  }

  "Valid translation" - {
    "is valid" in {
      assert(validTranslation("сёгун"))
    }

    "is valid in case of ellipsis" in {
      assert(validTranslation("или... или..."))
    }

    "is valid in case of one letter" in {
      assert(validTranslation("к"))
    }

    "is valid in case of brackets" in {
      assert(validTranslation("под водой (с маской или трубкой"))
    }

    "is valid with hyphen" in {
      assert(validTranslation("полу-автомат"))
    }

    "is valid for multiple meanings" in {
      assert(validTranslation("полу-автомат, устройство, автоматика"))
    }

    "is not valid with wrong start symbol" in {
      assert(!validTranslation(" полу-автомат, устройство, автоматика"))
    }

    "is not valid with wrong end symbol" in {
      assert(!validTranslation("полу-автомат, устройство, автоматика!"))
    }

    "is not valid with latin letters" in {
      assert(!validTranslation("value"))
    }
  }

  "Valid phrase" - {
    "is valid with ends with '.'" in {
      assert(validPhrase("It means 'да'.."))
    }

    "is valid with containing quotes and ends with query" in {
      assert(validPhrase("Ты о \"Scala\"??"))
    }

    "is valid with ends with percent" in {
      assert(validPhrase("He-got-3%"))
    }

    "is not valid with start from gap" in {
      assert(!validPhrase(" Not valid"))
    }

    "is not valid with ends on '^'" in {
      assert(!validPhrase("Not valid ^"))
    }

    "is not valid with '$' sign" in {
      assert(!validPhrase("Not $ valid"))
    }
  }

  "Valid transcription" - {
    "is valid" in {
      assert(validTranscription("[ðəmˈselvz]"))
    }

    "is not valid with numbers" in {
      assert(!validTranscription("[ðəmˈs23elvz]"))
    }

    "is not valid with punctuation" in {
      assert(!validTranscription("[ðəmˈselvz?]"))
    }

    "is not valid without brackets" in {
      assert(!validTranscription("ðəmˈselvz"))
    }
  }

  "Word" - {
    "is not valid by length" in {
      assert(!validValue("notvalidannotvalidannotvalidannotvalidannotvalidannotvalidan"))
    }
  }
}