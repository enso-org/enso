package org.enso.cli

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SpellingSpec extends AnyWordSpec with Matchers {
  "selectClosestMatches" should {
    "return close matches only" in {
      val matches = Spelling.selectClosestMatches(
        "spell",
        Seq("spill", "spool", "verydifferent")
      )

      matches.toSet shouldEqual Set("spill", "spool")
    }

    "return no matches if there are no remotely similar words" in {
      val matches = Spelling.selectClosestMatches(
        "spell",
        Seq("verydifferent")
      )

      matches should have size 0
    }
  }

  "selectClosestMatchesWithMetadata" should {
    "preserve metadata" in {
      val matches = Spelling.selectClosestMatchesWithMetadata(
        "spell",
        Seq(("spill", 1), ("spill", 2), ("verydifferent", 3))
      )

      matches.toSet shouldEqual Set(("spill", 1), ("spill", 2))
    }
  }
}
