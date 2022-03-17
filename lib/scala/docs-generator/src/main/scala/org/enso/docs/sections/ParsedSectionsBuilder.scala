package org.enso.docs.sections

import cats.kernel.Monoid
import cats.syntax.compose._
import org.enso.syntax.text.ast.Doc

final class ParsedSectionsBuilder {

  import ParsedSectionsBuilder._

  def build(doc: Doc): List[ParsedSection] = {
    val tagSections      = doc.tags.map(buildTags)
    val synopsisSections = doc.synopsis.map(buildSynopsis)
    val bodySections     = doc.body.map(buildBody)
    Monoid.combineAll(tagSections ++ synopsisSections ++ bodySections)
  }

  private def buildTags(tags: Doc.Tags): List[ParsedSection] =
    tags.elems.toList.map { tag =>
      Section.Tag(tag.name, tag.details.map(_.trim).map(Doc.Elem.Text))
    }

  private def buildSynopsis(synopsis: Doc.Synopsis): List[ParsedSection] =
    (joinSections _ >>> buildSections)(synopsis.elems.toList)

  private def buildBody(body: Doc.Body): List[ParsedSection] =
    (joinSections _ >>> buildSections)(body.elems.toList)

  private def buildSections(
    sections: List[Doc.Section]
  ): List[ParsedSection] = {
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

  private def buildMark(typ: Doc.Section.Marked.Type): Section.Mark =
    typ match {
      case Doc.Section.Marked.Important => Section.Mark.Important
      case Doc.Section.Marked.Info      => Section.Mark.Info
      case Doc.Section.Marked.Example   => Section.Mark.Example
    }

  private def joinSections(sections: List[Doc.Section]): List[Doc.Section] = {
    val init: Option[Doc.Section.Marked] = None
    val stack: List[Doc.Section]         = Nil

    val (result, acc) = sections.foldLeft((stack, init)) {
      case ((stack, acc), section) =>
        (section, acc) match {
          case (marked: Doc.Section.Marked, _) =>
            (acc.toList ::: stack, Some(marked))
          case (raw: Doc.Section.Raw, None) =>
            (raw :: stack, acc)
          case (raw: Doc.Section.Raw, Some(marked)) =>
            if (raw.indent == marked.indent) {
              val newElems = marked.elems ::: Doc.Elem.Newline :: raw.elems
              (stack, Some(marked.copy(elems = newElems)))
            } else {
              (raw :: marked :: stack, None)
            }
        }
    }

    (acc.toList ::: result).reverse
  }
}
object ParsedSectionsBuilder {

  object const {
    final val COLON = ':'
    final val SPACE = " "
  }
}
