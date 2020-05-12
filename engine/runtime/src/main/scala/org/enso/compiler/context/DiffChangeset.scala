package org.enso.compiler.context

import org.enso.compiler.core.IR
import org.enso.syntax.text.Location
import org.enso.text.editing.model.{Position, TextEdit}

import scala.collection.mutable

final class DiffChangeset {

  /**
    * Traverses the IR and returns a list of the most specific (the innermost)
    * IR identifiers affected by the edit by comparing the source locations.
    *
    * @param edit the text edit.
    * @param source the text source.
    * @param ir the IR node.
    * @return the list of IR identifiers affected by the edit.
    */
  def compute(
    edit: TextEdit,
    source: CharSequence,
    ir: IR
  ): Seq[IR.Identifier] = {
    @scala.annotation.tailrec
    def go(
      edit: Location,
      queue: mutable.Queue[IR],
      acc: mutable.Builder[IR.Identifier, Vector[IR.Identifier]]
    ): Seq[IR.Identifier] =
      if (queue.isEmpty) {
        acc.result()
      } else {
        val ir                  = queue.dequeue()
        val invalidatedChildren = ir.children.filter(intersect(edit, _))
        if (invalidatedChildren.isEmpty) {
          if (intersect(edit, ir)) {
            go(edit, queue, acc += ir.getId)
          } else {
            go(edit, queue ++= ir.children, acc)
          }
        } else {
          go(edit, queue ++= invalidatedChildren, acc)
        }
      }

    go(
      toLocation(edit, source),
      mutable.Queue(ir),
      Vector.newBuilder[IR.Identifier]
    )
  }

  /**
    * Checks if the IR is affected by the edit.
    *
    * @param edit location of the edit.
    * @param ir the IR node.
    * @return true if the node is affected by the edit.
    */
  def intersect(edit: Location, ir: IR): Boolean = {
    ir.location.map(_.location).exists(intersect(edit, _))
  }

  /**
    * Checks if the node location intersects the edit location.
    *
    * @param edit location of the edit.
    * @param node location of the node.
    * @return true if the node and edit locations are intersecting.
    */
  private def intersect(edit: Location, node: Location): Boolean =
    inside(node.start, edit) || inside(node.end, edit)

  /**
    * Checks if the character position index is inside the location.
    *
    * @param index the character position.
    * @param location the location.
    * @return true if the index is inside the location.
    */
  private def inside(index: Int, location: Location): Boolean =
    index >= location.start && index <= location.end

  /**
    * Converts [[TextEdit]] location to [[Location]] in the provided source.
    *
    * @param edit the text edit.
    * @param source the source text.
    * @return location of the text edit in the source text.
    */
  private def toLocation(edit: TextEdit, source: CharSequence): Location = {
    val start = edit.range.start
    val end   = edit.range.end
    Location(toIndex(start, source), toIndex(end, source))
  }

  /**
    * Converts position relative to a line to an absolute position in the
    * source.
    *
    * @param pos character position.
    * @param source the source text.
    * @return absolute position in the source.
    */
  private def toIndex(pos: Position, source: CharSequence): Int = {
    val prefix = source.toString.linesIterator.take(pos.line)
    prefix.mkString(System.lineSeparator()).length + pos.character
  }

}
