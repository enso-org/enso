package org.enso.compiler.context

import org.enso.docs.sections.{ParsedSectionsBuilder, Section}
import org.enso.polyglot.DocSection
import org.enso.syntax.text.DocParser
import org.enso.syntax.text.ast.Doc

final class DocSectionsBuilder(parsedSectionsBuilder: ParsedSectionsBuilder) {

  import DocSectionsBuilder._

  def build(comment: String): List[DocSection] = {
    val doc    = DocParser.runMatched(comment)
    val parsed = parsedSectionsBuilder.build(doc)
    parsed.map(render)
  }

  def render(section: Section): DocSection =
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

  private def buildMark(mark: Section.Mark): DocSection.Mark =
    mark match {
      case Section.Mark.Important => DocSection.Mark.Important()
      case Section.Mark.Info      => DocSection.Mark.Info()
      case Section.Mark.Example   => DocSection.Mark.Example()
    }
}
object DocSectionsBuilder {

  private val DivOpenLength  = 5
  private val DivCloseLength = 6

  private def renderHtml(elems: Doc.HTML): String =
    scalatags.Text.all
      .div(elems: _*)
      .toString
      .drop(DivOpenLength)
      .dropRight(DivCloseLength)

  private def renderElems(elems: Seq[Doc.Elem]): String = {
    val builder =
      elems.foldLeft(Seq.newBuilder[scalatags.Text.all.Modifier]) { (b, a) =>
        b ++= a.html
      }
    renderHtml(builder.result())
  }

}
