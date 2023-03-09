package org.enso.docs.sections

import org.enso.polyglot.DocSection
import org.enso.syntax.text.DocParser
import org.enso.syntax.text.ast.Doc

/** Module that splits the documentation into sections.
  *
  * @param parsedSectionsBuilder creates documenation sections from the parsed
  * docstring
  */
final class DocSectionsBuilder(parsedSectionsBuilder: ParsedSectionsBuilder) {

  import DocSectionsBuilder._

  /** Create the list of docsections from the documentation comment.
    *
    * @param comment the documentation comment
    * @return the list of documentation sections
    */
  def build(comment: String): List[DocSection] = {
    val doc    = DocParser.runMatched(comment)
    val parsed = parsedSectionsBuilder.build(doc)
    parsed.map(render)
  }

  /** Converts the docsection AST into a human-readable representation.
    *
    * @param section the docsection AST
    */
  private def render(section: Section): DocSection =
    section match {
      case Section.Tag(name, body) =>
        DocSection.Tag(name, renderElems(body))

      case Section.Paragraph(body) =>
        DocSection.Paragraph(renderElems(body))

      case Section.Keyed(key, body) =>
        DocSection.Keyed(key, renderElems(body))

      case Section.Marked(mark, header, body) =>
        DocSection.Marked(buildMark(mark), header, renderElems(body))
    }

  /** Convert the [[Section.Mark]] into [[DocSection.Mark]] representation.
    *
    * @param mark the section mark AST
    * @return the [[DocSection.Mark]] mark representation
    */
  private def buildMark(mark: Section.Mark): DocSection.Mark =
    mark match {
      case Section.Mark.Important => DocSection.Mark.Important()
      case Section.Mark.Info      => DocSection.Mark.Info()
      case Section.Mark.Example   => DocSection.Mark.Example()
    }
}
object DocSectionsBuilder {

  /** @return the instance of [[DocSectionsBuilder]]. */
  def apply(): DocSectionsBuilder =
    new DocSectionsBuilder(new ParsedSectionsBuilder)

  private def renderElems(elems: Seq[Doc.Elem]): String = {
    val builder =
      elems.foldLeft(Seq.newBuilder[scalatags.Text.all.Modifier]) { (b, a) =>
        b ++= HtmlRepr[Doc.Elem].toHtml(a)
      }
    HtmlRepr.renderHtml(builder.result())
  }
}
