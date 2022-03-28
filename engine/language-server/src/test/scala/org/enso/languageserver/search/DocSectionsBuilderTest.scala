package org.enso.languageserver.search

import org.enso.polyglot.DocSection
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DocSectionsBuilderTest extends AnyWordSpec with Matchers {

  import DocSectionsBuilderTest._

  "DocSectionsBuilder" should {

    "build example 1" in {
      val comment =
        """ ADVANCED
          | UNSTABLE
          | ALIAS foo, bar, baz
          |
          | Returns the method name of the method that could not be found.
          |
          | > Example
          |   Getting the method name from a no such method error.
          |
          |       import Standard.Examples
          |
          |       example_method_name =
          |           error = Examples.no_such_method
          |           error.method_name
          |""".stripMargin.linesIterator.mkString("\n")
      val expected = Seq(
        DocSection.Tag("ADVANCED", ""),
        DocSection.Tag("UNSTABLE", ""),
        DocSection.Tag("ALIAS", "foo, bar, baz"),
        DocSection.Paragraph(
          "Returns the method name of the method that could not be found. "
        ),
        DocSection.Marked(
          DocSection.Mark.Example(),
          Some("Example"),
          " Getting the method name from a no such method error. <pre><code>import Standard.Examples</code><br /><code>example_method_name =</code><br /><code>    error = Examples.no_such_method</code><br /><code>    error.method_name</code><br /></pre>"
        )
      )

      build(comment) shouldEqual expected
    }

    "build example 2" in {
      val comment =
        """ ADVANCED
          |
          | A function that can be used to indicate that something hasn't been
          | implemented yet.
          |
          | Arguments:
          | - message: A description of what implementation is missing.
          |
          | > Example
          |   Throwing an error to show that something is unimplemented.
          |
          |       import Standard.Base.Error.Extensions
          |
          |       example_unimplemented = Extensions.unimplemented
          |""".stripMargin.linesIterator.mkString("\n")
      val expected = Seq(
        DocSection.Tag("ADVANCED", ""),
        DocSection.Paragraph(
          "A function that can be used to indicate that something hasn't been implemented yet. "
        ),
        DocSection.Keyed(
          "Arguments",
          " <ul><li>message: A description of what implementation is missing.</li></ul> "
        ),
        DocSection.Marked(
          DocSection.Mark.Example(),
          Some("Example"),
          " Throwing an error to show that something is unimplemented. <pre><code>import Standard.Base.Error.Extensions</code><br /><code>example_unimplemented = Extensions.unimplemented</code><br /></pre>"
        )
      )

      build(comment) shouldEqual expected
    }

    "build example 3" in {
      val comment =
        """ ADVANCED
          |
          | Converts a polyglot value representing an array into a vector.
          |
          | Arguments:
          | - arr: The polyglot array value to wrap into a vector.
          |
          | This is useful when wrapping polyglot APIs for further use in Enso.
          |
          | A vector allows to store an arbitrary number of elements in linear memory. It
          | is the recommended data structure for most applications.
          |
          | ! Value Copying
          |   As Enso vectors implement immutable semantics, this constructor function
          |   makes a copy of each value in the argument array.
          |
          |   If this didn't happen then it would be possible for the underlying array to
          |   be mutated under the hood, and sneak mutability into our immutable data.
          |""".stripMargin.linesIterator.mkString("\n")
      val expected = Seq(
        DocSection.Tag("ADVANCED", ""),
        DocSection.Paragraph(
          "Converts a polyglot value representing an array into a vector. "
        ),
        DocSection.Keyed(
          "Arguments",
          " <ul><li>arr: The polyglot array value to wrap into a vector.</li></ul> "
        ),
        DocSection.Paragraph(
          "This is useful when wrapping polyglot APIs for further use in Enso. "
        ),
        DocSection.Paragraph(
          "A vector allows to store an arbitrary number of elements in linear memory. It is the recommended data structure for most applications. "
        ),
        DocSection.Marked(
          DocSection.Mark.Important(),
          Some("Value Copying"),
          " As Enso vectors implement immutable semantics, this constructor function makes a copy of each value in the argument array.  If this didn't happen then it would be possible for the underlying array to be mutated under the hood, and sneak mutability into our immutable data."
        )
      )

      build(comment) shouldEqual expected
    }

    "build example 4" in {
      val comment =
        """ Returns the larger value of `this` and `that`.
          |
          | Arguments:
          | - that: The number to compare `this` against.
          |
          | ? Math.max or Number.max
          |   While we provide the max method on `Number`, we find it more intuitive to
          |   write `Math.max a b` rather than `a.max b`. To that end, we recommend using
          |   the first style.
          |
          | > Example
          |   Find the maximum of 2 and 5.
          |
          |       2.max 5
          |""".stripMargin.linesIterator.mkString("\n")
      val expected = Seq(
        DocSection.Paragraph(
          "Returns the larger value of <code>this</code> and <code>that</code>. "
        ),
        DocSection.Keyed(
          "Arguments",
          " <ul><li>that: The number to compare <code>this</code> against.</li></ul> "
        ),
        DocSection.Marked(
          DocSection.Mark.Info(),
          Some("Math.max or Number.max"),
          " While we provide the max method on <code>Number</code>, we find it more intuitive to write <code>Math.max a b</code> rather than <code>a.max b</code>. To that end, we recommend using the first style. "
        ),
        DocSection.Marked(
          DocSection.Mark.Example(),
          Some("Example"),
          " Find the maximum of 2 and 5. <pre><code>2.max 5</code><br /></pre>"
        )
      )

      build(comment) shouldEqual expected
    }

    "build example 5" in {
      val comment =
        """ PRIVATE
          | A key-value store. This type assumes all keys are pairwise comparable,
          | using the `<`, `>` and `==` operators.
          |
          | Arguments:
          | - s: The size of the tree at this node.
          | - key: The key stored at this node.
          | - value: The value stored at this node.
          | - left: The left subtree.
          | - right: The right subtree.
          |""".stripMargin.linesIterator.mkString("\n")
      val expected = Seq(
        DocSection.Tag("PRIVATE", ""),
        DocSection.Paragraph(
          "A key-value store. This type assumes all keys are pairwise comparable, using the <code>&lt;</code>, <code>&gt;</code> and <code>==</code> operators. "
        ),
        DocSection.Keyed(
          "Arguments",
          " <ul><li>s: The size of the tree at this node.</li><li>key: The key stored at this node.</li><li>value: The value stored at this node.</li><li>left: The left subtree.</li><li>right: The right subtree.</li></ul>"
        )
      )

      build(comment) shouldEqual expected
    }
  }
}
object DocSectionsBuilderTest {

  val builder: DocSectionsBuilder =
    DocSectionsBuilder()

  def build(comment: String): Seq[DocSection] =
    builder.build(comment)
}
