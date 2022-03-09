package org.enso.docs.generator

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DocSectionsGeneratorTest extends AnyWordSpec with Matchers {

  "DocSectionsGenerator" should {

    "generate single tag" in {
      val comment =
        """ UNSTABLE
          |""".stripMargin
      val expected = DocSections(Some(Seq("UNSTABLE")), None)

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate multiple tags" in {
      val comment =
        """ UNSTABLE
          |DEPRECATED
          |""".stripMargin
      val expected = DocSections(Some(Seq("UNSTABLE", "DEPRECATED")), None)

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate synopsis single line" in {
      val comment =
        """ hello world
          |""".stripMargin
      val expected = DocSections(None, Some("<p>hello world</p>"))

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate synopsis multiline" in {
      val comment =
        """ hello world
          |multiline
          |""".stripMargin
      val expected = DocSections(None, Some("<p>hello world multiline</p>"))

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

  }
}
