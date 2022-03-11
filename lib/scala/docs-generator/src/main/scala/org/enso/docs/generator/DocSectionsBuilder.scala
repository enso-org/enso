package org.enso.docs.generator

import cats.kernel.Monoid
import org.enso.syntax.text.ast.Doc

final class DocSectionsBuilder {

  import DocSectionsBuilder._

  def build(doc: Doc): List[DocSection] = {
    val tagSections      = doc.tags.map(buildTags)
    val synopsisSections = doc.synopsis.map(buildSynopsis)
    val bodySections     = doc.body.map(buildBody)
    Monoid.combineAll(tagSections ++ synopsisSections ++ bodySections)
  }

  def buildTags(tags: Doc.Tags): List[DocSection.Tag] =
    tags.elems.toList.map { tag =>
      DocSection.Tag(tag.name, tag.details.map(_.trim))
    }

  def buildSynopsis(synopsis: Doc.Synopsis): List[DocSection] =
    buildSections(synopsis.elems.toList)

  def buildBody(body: Doc.Body): List[DocSection] =
    buildSections(body.elems.toList)

  def buildSections(sections: List[Doc.Section]): List[DocSection] = {
    sections.map {
      case Doc.Section.Raw(_, elems) =>
        elems match {
          case Doc.Elem.Text(text) :: t =>
            val (key, value) = text.span(_ != const.COLON)
            if (value.nonEmpty) {
              val line = value.drop(1).stripPrefix(" ")
              val body = if (line.isEmpty) t else Doc.Elem.Text(line) :: t
              DocSection.Keyed(key, body)
            } else {
              DocSection.Paragraph(elems)
            }
          case _ =>
            DocSection.Paragraph(elems)
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
            DocSection.Marked(buildMark(typ), header, tail)
          case Nil =>
            DocSection.Marked(buildMark(typ), None, elems)
        }
    }
  }

  def buildMark(typ: Doc.Section.Marked.Type): DocSection.Mark =
    typ match {
      case Doc.Section.Marked.Important => DocSection.Mark.Important
      case Doc.Section.Marked.Info      => DocSection.Mark.Info
      case Doc.Section.Marked.Example   => DocSection.Mark.Example
    }
}
object DocSectionsBuilder {

  object const {
    final val COLON = ':'
  }
}

sealed trait DocSection
object DocSection {

  case class Tag(name: String, text: Option[String])  extends DocSection
  case class Paragraph(body: List[Doc.Elem])          extends DocSection
  case class Keyed(key: String, body: List[Doc.Elem]) extends DocSection
  case class Marked(mark: Mark, header: Option[String], body: List[Doc.Elem])
      extends DocSection

  sealed trait Mark
  object Mark {
    case object Important extends Mark
    case object Info      extends Mark
    case object Example   extends Mark
  }
}
