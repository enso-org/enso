package org.enso.docs.sections
import org.enso.syntax.text.ast.Doc

/** The base trait for the section. */
sealed trait Section
object Section {

  /** The documentation tag.
    *
    * {{{
    *   name text
    * }}}
    *
    * ==Example==
    *
    * {{{
    *   UNSTABLE
    *   DEPRECATED
    *   ALIAS Length
    * }}}
    *
    * @param name the tag name
    * @param body the tag text
    */
  case class Tag(name: String, body: List[Doc.Elem]) extends Section

  /** The paragraph of the text.
    *
    * ==Example==
    *
    * {{{
    *   Arbitrary text in the documentation comment.
    *
    *   This is another paragraph.
    * }}}
    *
    * @param body the elements that make up this paragraph
    */
  case class Paragraph(body: List[Doc.Elem]) extends Section

  /** The section that starts with the key followed by the colon and the body.
    *
    * {{{
    *   key: body
    * }}}
    *
    * ==Example==
    *
    * {{{
    *   Arguments:
    *   - one: the first
    *   - two: the second
    * }}}
    *
    * {{{
    *   Icon: table-from-rows
    * }}}
    *
    * @param key the section key
    * @param body the elements the make up the body of the section
    */
  case class Keyed(key: String, body: List[Doc.Elem]) extends Section

  /** The section that starts with the mark followed by the header and the body.
    *
    * {{{
    *   mark header
    *   body
    * }}}
    *
    * ==Example==
    *
    * {{{
    *   > Example
    *     This is how it's done.
    *         foo = bar baz
    * }}}
    *
    * {{{
    *   ! Notice
    *     This is important.
    * }}}
    */
  case class Marked(mark: Mark, header: Option[String], body: List[Doc.Elem])
      extends Section

  /** The base trait for the section marks. */
  sealed trait Mark
  object Mark {
    case object Important extends Mark
    case object Info      extends Mark
    case object Example   extends Mark
  }
}
