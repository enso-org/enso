package org.enso.docs.generator

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DocSectionsGeneratorTest extends AnyWordSpec with Matchers {

  "DocSectionsGenerator" should {

    "generate single tag" in {
      val comment =
        """ UNSTABLE
          |""".stripMargin
      val expected =
        DocSections(
          Some(DocTags(Seq(DocTag("UNSTABLE", None)))),
          None,
          None,
          None,
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate multiple tags" in {
      val comment =
        """ UNSTABLE
          | DEPRECATED
          |""".stripMargin
      val expected =
        DocSections(
          Some(
            DocTags(Seq(DocTag("UNSTABLE", None), DocTag("DEPRECATED", None)))
          ),
          None,
          None,
          None,
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate tag with description" in {
      val comment =
        """ ALIAS Check Matches
          |""".stripMargin
      val expected =
        DocSections(
          Some(DocTags(Seq(DocTag("ALIAS", Some("Check Matches"))))),
          None,
          None,
          None,
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate synopsis single line" in {
      val comment =
        """ hello world
          |""".stripMargin
      val expected =
        DocSections(None, Some("<p>hello world</p>"), None, None, None, None)

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate synopsis multiline" in {
      val comment =
        """ hello world
          | multiline
          |""".stripMargin
      val expected = DocSections(
        None,
        Some("<p>hello world multiline</p>"),
        None,
        None,
        None,
        None
      )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate arguments" in {
      val comment =
        """ Synopsis
          |
          | Arguments:
          | - one: The first
          |   second line
          | - two: The second
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("<p>Synopsis </p>"),
          Some(
            DocArguments(
              Seq(
                DocArgument("one", "The first\n   second line"),
                DocArgument("two", "The second")
              )
            )
          ),
          None,
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate single example single line" in {
      val comment =
        """ Synopsis
          |
          | > Example
          | First line
          |     example =
          |         1
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("<p>Synopsis </p>"),
          None,
          Some(
            DocExamples(
              Seq(
                DocExample(
                  "First line",
                  """example =
                    |    1""".stripMargin
                )
              )
            )
          ),
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate single example multiline" in {
      val comment =
        """ Synopsis
          |
          | > Example
          | First line
          | Second line
          |
          |     import Standard.Table
          |
          |     example =
          |         1
          |         2
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("<p>Synopsis </p>"),
          None,
          Some(
            DocExamples(
              Seq(
                DocExample(
                  "First line Second line",
                  """import Standard.Table
                    |example =
                    |    1
                    |    2""".stripMargin
                )
              )
            )
          ),
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate multiple examples" in {
      val comment =
        """ Synopsis
          |
          | > Example
          | First line
          |     example =
          |         1
          |
          | > Example
          |   Second example
          |
          |       import Standard.Base
          |
          |       main =
          |           IO.println "Hello World!"
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("<p>Synopsis </p>"),
          None,
          Some(
            DocExamples(
              Seq(
                DocExample(
                  "First line",
                  """example =
                    |    1""".stripMargin
                ),
                DocExample(
                  "Second example",
                  """import Standard.Base
                    |main =
                    |    IO.println "Hello World!"""".stripMargin
                )
              )
            )
          ),
          None,
          None
        )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }
  }
}
