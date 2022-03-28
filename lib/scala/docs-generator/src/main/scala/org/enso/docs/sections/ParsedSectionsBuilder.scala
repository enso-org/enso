package org.enso.docs.sections

import cats.kernel.Monoid
import cats.syntax.compose._
import org.enso.syntax.text.ast.Doc

/** Combine the documentation into a list of [[Section]]s. */
final class ParsedSectionsBuilder {

  import ParsedSectionsBuilder._

  /** Build the parsed sections from the provided documentation comment.
    *
    * @param doc the parsed documentation comment.
    * @return the list of parsed sections.
    */
  def build(doc: Doc): List[Section] = {
    val tagSections      = doc.tags.map(buildTags)
    val synopsisSections = doc.synopsis.map(buildSynopsis)
    val bodySections     = doc.body.map(buildBody)
    Monoid.combineAll(tagSections ++ synopsisSections ++ bodySections)
  }

  /** Process the tags section of the documentation comment.
    *
    * @param tags the tags section
    * @return the list of parsed sections
    */
  private def buildTags(tags: Doc.Tags): List[Section] =
    tags.elems.toList.map { tag =>
      Section.Tag(tag.name, tag.details.map(_.trim).map(Doc.Elem.Text).toList)
    }

  /** Process the synopsis section of the documentation comment.
    *
    * @param synopsis the synopsis section
    * @return the list of parsed sections
    */
  private def buildSynopsis(synopsis: Doc.Synopsis): List[Section] =
    (preprocess >>> buildSections)(synopsis.elems.toList)

  /** Process the body section of the documentation comment.
    *
    * @param body the body section
    * @return the list of parsed sections
    */
  private def buildBody(body: Doc.Body): List[Section] =
    (preprocess >>> buildSections)(body.elems.toList)

  /** Process the list of [[Doc.Section]] documentation sections.
    *
    * @param sections the list of parsed documentation sections
    * @return the list of parsed sections
    */
  private def buildSections(sections: List[Doc.Section]): List[Section] =
    sections.map {
      case Doc.Section.Raw(_, elems) =>
        elems match {
          case Doc.Elem.Text(text) :: tail =>
            val (key, value) = text.span(_ != const.COLON)
            if (value.nonEmpty) {
              val line = value.drop(1).stripPrefix(const.SPACE)
              val body = if (line.isEmpty) tail else Doc.Elem.Text(line) :: tail
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

  /** Create the [[Section.Mark]] from the [[Doc.Section.Marked.Type]]
    * section type.
    *
    * @param typ the type of documentation section
    * @return the corresponding section mark
    */
  private def buildMark(typ: Doc.Section.Marked.Type): Section.Mark =
    typ match {
      case Doc.Section.Marked.Important => Section.Mark.Important
      case Doc.Section.Marked.Info      => Section.Mark.Info
      case Doc.Section.Marked.Example   => Section.Mark.Example
    }

  /** The preprocessor function that is invoked before building the
    * resulting list of sections.
    */
  private def preprocess: List[Doc.Section] => List[Doc.Section] =
    joinSections _ >>> filterSections

  /** Preprocess the list of documentation sections and join the paragraphs of
    * the same offset with the marked section.
    *
    * ==Example==
    * In the parsed [[Doc.Section]], the "Some paragraph" is a separate section,
    * while having the same indentation. This pass joins them into a single
    * section.
    *
    * {{{
    *   ? Info
    *     Some info.
    *
    *     Some paragraph.
    * }}}
    *
    * @param sections the list of documentation sections
    * @return preprocessed list of documentation sections with the
    *         paragraphs joined into the corresponding marked sections.
    */
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

  /** Filter the sections before processing them.
    *
    * The function filters out:
    * - empty sections
    *
    * @param sections the list of documentation sections
    * @return the list of filtered sections
    */
  private def filterSections(sections: List[Doc.Section]): List[Doc.Section] =
    sections
      .filter {
        case Doc.Section.Raw(_, List(Doc.Elem.Newline)) => false
        case _                                          => true
      }
}

object ParsedSectionsBuilder {

  object const {
    final val COLON = ':'
    final val SPACE = " "
  }
}
