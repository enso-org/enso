package org.enso.polyglot

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

/** The base trait for the documentation section. */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[DocSection.Tag],
      name  = "docsectionTag"
    ),
    new JsonSubTypes.Type(
      value = classOf[DocSection.Paragraph],
      name  = "docsectionParagraph"
    ),
    new JsonSubTypes.Type(
      value = classOf[DocSection.Keyed],
      name  = "docsectionKeyed"
    ),
    new JsonSubTypes.Type(
      value = classOf[DocSection.Marked],
      name  = "docsectionMarked"
    )
  )
)
sealed trait DocSection
object DocSection {

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
  case class Tag(name: String, body: String) extends DocSection

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
  case class Paragraph(body: String) extends DocSection

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
  case class Keyed(key: String, body: String) extends DocSection

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
    *
    * @param mark the section mark
    * @param header the section header
    * @param body the elements that make up the body of the section
    */
  case class Marked(mark: Mark, header: Option[String], body: String)
      extends DocSection

  /** The base trait for the section marks. */
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
  @JsonSubTypes(
    Array(
      new JsonSubTypes.Type(
        value = classOf[Mark.Important],
        name  = "docsectionMarkImportant"
      ),
      new JsonSubTypes.Type(
        value = classOf[Mark.Info],
        name  = "docsectionMarkInfo"
      ),
      new JsonSubTypes.Type(
        value = classOf[Mark.Example],
        name  = "docsectionMarkExample"
      )
    )
  )
  sealed trait Mark
  object Mark {
    case class Important() extends Mark
    case class Info()      extends Mark
    case class Example()   extends Mark
  }
}
