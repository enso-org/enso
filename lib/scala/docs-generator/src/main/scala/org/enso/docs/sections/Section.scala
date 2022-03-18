package org.enso.docs.sections

/** The base trait for the section. */
sealed trait Section[A]
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
    * @param text the tag text
    */
  case class Tag[A](name: String, text: Option[A]) extends Section[A]

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
  case class Paragraph[A](body: List[A]) extends Section[A]

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
  case class Keyed[A](key: String, body: List[A]) extends Section[A]

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
  case class Marked[A](mark: Mark, header: Option[String], body: List[A])
      extends Section[A]

  /** The base trait for the section marks. */
  sealed trait Mark
  object Mark {
    case object Important extends Mark
    case object Info      extends Mark
    case object Example   extends Mark
  }
}
