package org.enso.docs.sections

import org.enso.syntax.text.ast.Doc
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HtmlReprTest extends AnyWordSpec with Matchers {

  import HtmlReprTest._

  "HtmlRepr" should {

    "render Unclosed italic" in {
      val elem = Doc.Elem.Formatter.Unclosed(Doc.Elem.Formatter.Italic, "hello")

      render(elem) shouldEqual "_hello"
    }

    "render Unclosed bold" in {
      val elem = Doc.Elem.Formatter.Unclosed(Doc.Elem.Formatter.Bold, "hello")

      render(elem) shouldEqual "*hello"
    }

    "render code block" in {
      val elem = Doc.Elem.CodeBlock(
        Doc.Elem.CodeBlock.Line(0, "main ="),
        Doc.Elem.CodeBlock.Line(4, """"Hello World!"""")
      )

      render(elem) shouldEqual
      "<pre><code>main =</code><br /><code>    &quot;Hello World!&quot;</code><br /></pre>"
    }

    "render inline code block" in {
      val elem = Doc.Elem.CodeBlock.Inline("foo = bar baz")

      render(elem) shouldEqual "<code>foo = bar baz</code>"
    }

    "render code block line" in {
      val elem = Doc.Elem.CodeBlock.Line(2, "foo = bar baz")

      render(elem) shouldEqual "<code>  foo = bar baz</code><br />"
    }

    "render Formatter italic" in {
      val elem = Doc.Elem.Formatter(
        Doc.Elem.Formatter.Italic,
        "hello"
      )

      render(elem) shouldEqual "<i>hello</i>"
    }

    "render Formatter strikeout" in {
      val elem = Doc.Elem.Formatter(
        Doc.Elem.Formatter.Strikeout,
        "world"
      )

      render(elem) shouldEqual "<s>world</s>"
    }

    "render header section" in {
      val elem = Doc.Section.Header("Section")

      render(elem) shouldEqual "<h1>Section</h1>"
    }

    "render URL link" in {
      val elem = Doc.Elem.Link.URL("hello", "https://example.com/hello")

      render(elem) shouldEqual
      """<a href="https://example.com/hello">hello</a>"""
    }

    "render image link" in {
      val elem = Doc.Elem.Link.Image("hello", "https://example.com/hello.png")

      render(elem) shouldEqual
      """<img src="https://example.com/hello.png" />hello"""
    }

    "render deprecated tag" in {
      val elem = Doc.Tags.Tag(2, Doc.Tags.Tag.Type.Deprecated)

      render(elem) shouldEqual "DEPRECATED"
    }

    "render alias tag" in {
      val elem = Doc.Tags.Tag(2, Doc.Tags.Tag.Type.Alias, "Other function")

      render(elem) shouldEqual "ALIAS Other function"
    }

    "render lists" in {
      val elem = Doc.Elem.List(
        2,
        Doc.Elem.List.Unordered,
        Doc.Elem.ListItem("first"),
        Doc.Elem.List(
          4,
          Doc.Elem.List.Ordered,
          Doc.Elem.ListItem("a"),
          Doc.Elem.ListItem("b"),
          Doc.Elem.MisalignedItem(3, Doc.Elem.List.Ordered, "c")
        ),
        Doc.Elem.ListItem("second")
      )

      render(elem) shouldEqual
      "<ul><li>first</li><ol><li>a</li><li>b</li><li>c</li></ol><li>second</li></ul>"
    }

    "render lists formatted" in {
      val elem = Doc.Elem.List(
        0,
        Doc.Elem.List.Unordered,
        Doc.Elem.ListItem(
          Doc.Elem.Formatter(Doc.Elem.Formatter.Italic, "one"),
          ": the first"
        ),
        Doc.Elem.ListItem(
          "two",
          Doc.Elem.Formatter.Unclosed(
            Doc.Elem.Formatter.Italic,
            "three",
            ": the ",
            Doc.Elem.Formatter(Doc.Elem.Formatter.Bold, "second")
          )
        )
      )

      render(
        elem
      ) shouldEqual "<ul><li><i>one</i>: the first</li><li>two_three: the <b>second</b></li></ul>"
    }
  }
}
object HtmlReprTest {

  def render(elem: Doc.Elem): String = {
    val html = HtmlRepr[Doc.Elem].toHtml(elem)
    HtmlRepr.renderHtml(html)
  }
}
