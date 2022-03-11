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

    "generate description single line" in {
      val comment =
        """ hello world
          |""".stripMargin
      val expected =
        DocSections(None, Some("hello world"), None, None, None, None)

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate description multiline" in {
      val comment =
        """ hello world
          | multiline
          |""".stripMargin
      val expected = DocSections(
        None,
        Some("hello world multiline"),
        None,
        None,
        None,
        None
      )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate description with markdown" in {
      val comment =
        """ hello world `foo` _bar_
          |""".stripMargin
      val expected = DocSections(
        None,
        Some("hello world <code>foo</code> <i>bar</i>"),
        None,
        None,
        None,
        None
      )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate description with important sections" in {
      val comment =
        """ Description
          |
          | Some text
          | Second line
          |
          | Arguments:
          | - one: one
          |
          | > Example
          |   Example1
          |       main = 42
          |
          | ! Bang
          |   `here` or ~there~
          |
          | Some other text
          |
          | Icon: my-icon
          |""".stripMargin
      val expected = DocSections(
        None,
        Some("Description"),
        None,
        None,
        None,
        None
      )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate arguments simple" in {
      val comment =
        """ Description
          |
          | Arguments:
          | - one: The first
          |   second line
          | - two: The second
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("Description"),
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

    "generate arguments with markdown" in {
      // TODO: parser does not support markdown in lists
      pending
      val comment =
        """ Description
          |
          | Arguments:
          | - pattern: The pattern to match `this` against _raw text_
          |   - sublist item
          | - two: bar
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("Description"),
          Some(
            DocArguments(
              Seq(
                DocArgument(
                  "pattern",
                  "This pattern to match <code>this</code> against <i>raw text</i><ul><li>sublist item</li></ul>"
                )
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
        """ Description
          |
          | > Example
          | First line
          |     example =
          |         1
          |""".stripMargin
      val expected =
        DocSections(
          None,
          Some("Description"),
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
        """ Description
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
          Some("Description"),
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
          Some("Synopsis"),
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

    "generate icon section" in {
      val comment =
        """ Description
          |
          | Icon: table-from-rows
          |""".stripMargin
      val expected = DocSections(
        None,
        Some("Description"),
        None,
        None,
        Some("table-from-rows"),
        None
      )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

    "generate aliases section" in {
      val comment =
        """ Description
          |
          | Aliases: foo, bar baz, redshift®
          |""".stripMargin
      val expected = DocSections(
        None,
        Some("Description"),
        None,
        None,
        None,
        Some(DocAliases(Seq("foo", "bar baz", "redshift®")))
      )

      DocParserWrapper.generateSections(comment) shouldEqual expected
    }

  }
}
