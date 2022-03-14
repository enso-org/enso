package org.enso.docs.sections

import cats.kernel.Monoid
import org.enso.syntax.text.ast.Doc

final class ParsedSectionsBuilder {

  import ParsedSectionsBuilder._

  def build(doc: Doc): List[ParsedSection] = {
    val tagSections      = doc.tags.map(buildTags)
    val synopsisSections = doc.synopsis.map(buildSynopsis)
    val bodySections     = doc.body.map(buildBody)
    Monoid.combineAll(tagSections ++ synopsisSections ++ bodySections)
  }

  def buildTags(tags: Doc.Tags): List[ParsedSection] =
    tags.elems.toList.map { tag =>
      Section.Tag(tag.name, tag.details.map(_.trim).map(Doc.Elem.Text))
    }

  def buildSynopsis(synopsis: Doc.Synopsis): List[ParsedSection] =
    buildSections(synopsis.elems.toList)

  def buildBody(body: Doc.Body): List[ParsedSection] =
    buildSections(body.elems.toList)

  def buildSections(sections: List[Doc.Section]): List[ParsedSection] = {
    sections.map {
      case Doc.Section.Raw(_, elems) =>
        elems match {
          case Doc.Elem.Text(text) :: t =>
            val (key, value) = text.span(_ != const.COLON)
            if (value.nonEmpty) {
              val line = value.drop(1).stripPrefix(const.SPACE)
              val body = if (line.isEmpty) t else Doc.Elem.Text(line) :: t
              Section.Keyed(key, body)
            } else {
              Section.Paragraph(elems)
            }
          case _ =>
            Section.Paragraph(elems)
        }
      case Doc.Section.Marked(_, _, typ, elems) =>
        elems match {
          case head :: tail =>
            val header = head match {
              case header: Doc.Section.Header =>
                Some(header.repr.build())
              case _ =>
                None
            }
            Section.Marked(buildMark(typ), header, tail)
          case Nil =>
            Section.Marked(buildMark(typ), None, elems)
        }
    }
  }

  def buildMark(typ: Doc.Section.Marked.Type): Section.Mark =
    typ match {
      case Doc.Section.Marked.Important => Section.Mark.Important
      case Doc.Section.Marked.Info      => Section.Mark.Info
      case Doc.Section.Marked.Example   => Section.Mark.Example
    }
}
object ParsedSectionsBuilder {

  object const {
    final val COLON = ':'
    final val SPACE = " "
  }
}
