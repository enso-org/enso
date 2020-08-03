package org.enso.compiler.context

import java.util.UUID

import org.enso.compiler.core.IR
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.syntax.text.Location
import org.enso.text.editing.model.TextEdit
import org.enso.text.editing.{IndexedSource, TextEditor}

import scala.collection.mutable

/** Compute invalidated expressions.
  *
  * @param source the text source
  * @param ir the IR node
  * @tparam A the source type
  */
final class Changeset[A: TextEditor: IndexedSource](val source: A, val ir: IR) {

  /** Traverses the IR and returns a list of all IR nodes affected by the edit
    * using the [[DataflowAnalysis]] information.
    *
    * @param edits the text edits
    * @throws CompilerError if the IR is missing DataflowAnalysis metadata
    * @return the set of all IR nodes affected by the edit
    */
  @throws[CompilerError]
  def compute(edits: Seq[TextEdit]): Set[IR.ExternalId] = {
    val metadata = ir
      .unsafeGetMetadata(
        DataflowAnalysis,
        "Empty dataflow analysis metadata during changeset calculation."
      )
    val direct = invalidated(edits)
    val transitive = direct
      .map(Changeset.toDataflowDependencyType)
      .flatMap(metadata.getExternal)
      .flatten
    direct.flatMap(_.externalId) ++ transitive
  }

  /** Traverses the IR and returns a list of the most specific (the innermost)
    * IR nodes directly affected by the edit by comparing the source locations.
    *
    * @param edits the text edits
    * @return the set of IR nodes directly affected by the edit
    */
  def invalidated(edits: Seq[TextEdit]): Set[Changeset.NodeId] = {
    @scala.annotation.tailrec
    def go(
      tree: Changeset.Tree,
      source: A,
      edits: mutable.Queue[TextEdit],
      ids: mutable.Set[Changeset.NodeId]
    ): Set[Changeset.NodeId] = {
      if (edits.isEmpty) ids.toSet
      else {
        val edit           = edits.dequeue()
        val locationEdit   = Changeset.toLocationEdit(edit, source)
        val invalidatedSet = Changeset.invalidated(tree, locationEdit.location)
        val newTree        = Changeset.updateLocations(tree, locationEdit)
        val newSource      = TextEditor[A].edit(source, edit)
        go(newTree, newSource, edits, ids ++= invalidatedSet.map(_.id))
      }
    }
    val tree = Changeset.buildTree(ir)
    go(tree, source, mutable.Queue.from(edits), mutable.HashSet())
  }

  /** Apply the list of edits to the source file.
    *
    * @param edits the text edits
    * @return the source file after applying the edits
    */
  def applyEdits(edits: Iterable[TextEdit]): A =
    edits.foldLeft(source)(TextEditor[A].edit)

}

object Changeset {

  /** An identifier of IR node.
    *
    * @param internalId internal IR id
    * @param externalId external IR id
    */
  case class NodeId(
    internalId: IR.Identifier,
    externalId: Option[IR.ExternalId]
  )

  object NodeId {

    /** Create a [[NodeId]] identifier from [[IR]].
      *
      * @param ir the source IR
      * @return the identifier
      */
    def apply(ir: IR): NodeId =
      new NodeId(ir.getId, ir.getExternalId)

    implicit val ordering: Ordering[NodeId] = (x: NodeId, y: NodeId) => {
      val cmpInternal = Ordering[UUID].compare(x.internalId, y.internalId)
      if (cmpInternal == 0) {
        Ordering[Option[UUID]].compare(x.externalId, y.externalId)
      } else {
        cmpInternal
      }
    }
  }

  // === Changeset Internals ==================================================

  /** Internal representation of an [[IR]]. */
  private type Tree = mutable.TreeSet[Node]

  /** The location that has been edited.
    *
    * @param location the location of the edit
    * @param length the length of the inserted text
    */
  private case class LocationEdit(location: Location, length: Int) {

    /** The difference in length between the edited text and the inserted text.
      * Determines how much the rest of the text will be shifted after applying
      * the edit.
      */
    val locationDifference: Int = {
      val editRange = location.end - location.start
      length - editRange
    }
  }

  /** Internal representation of an `IR` node in the changeset.
    *
    * @param id the node id
    * @param location the node location
    */
  private case class Node(id: NodeId, location: Location) {

    /** Shift the node location.
      *
      * @param offset the offset relative to the previous node location
      * @return the node with a new location
      */
    def shift(offset: Int): Node = {
      val newLocation = location.copy(
        start = location.start + offset,
        end   = location.end + offset
      )
      copy(location = newLocation)
    }
  }

  private object Node {

    /** Create a node from [[IR]].
      *
      * @param ir the source IR
      * @return the node if `ir` contains a location
      */
    def fromIr(ir: IR): Option[Node] =
      ir.location.map(loc => Node(NodeId(ir), loc.location))

    /** Create an artificial node with fixed [[NodeId]]. It is used to select
      * nodes by location in the tree.
      *
      * @param location the location of the node
      * @return a select node
      */
    def select(location: Location): Node =
      new Node(NodeId(UUID.nameUUIDFromBytes(Array()), None), location)

    implicit val ordering: Ordering[Node] = (x: Node, y: Node) => {
      val compareStart =
        Ordering[Int].compare(x.location.start, y.location.start)
      if (compareStart == 0) {
        val compareEnd = Ordering[Int].compare(y.location.end, x.location.end)
        if (compareEnd == 0) Ordering[NodeId].compare(x.id, y.id)
        else compareEnd
      } else compareStart
    }
  }

  /** Build an internal representation of the [[IR]].
    *
    * @param ir the source IR
    * @return the tree representation of the IR
    */
  private def buildTree(ir: IR): Tree = {
    @scala.annotation.tailrec
    def go(input: mutable.Queue[IR], acc: Tree): Tree =
      if (input.isEmpty) acc
      else {
        val ir = input.dequeue()
        if (ir.children.isEmpty) {
          Node.fromIr(ir).foreach(acc.add)
        }
        go(input ++= ir.children, acc)
      }
    go(mutable.Queue(ir), mutable.TreeSet())
  }

  /** Update the tree locations after applying the edit.
    *
    * @param tree the source tree
    * @param edit the edit to apply
    * @return the tree with updated locations
    */
  private def updateLocations(tree: Tree, edit: LocationEdit): Tree = {
    val range = tree.rangeFrom(Node.select(edit.location)).toSeq
    range.foreach { updated =>
      tree -= updated
      tree += updated.shift(edit.locationDifference)
    }
    tree
  }

  /** Calculate the invalidated subset of the tree affected by the edit by
    * comparing the source locations.
    *
    * @param tree the source tree
    * @param edit the location of the edit
    * @return the invalidated nodes of the tree
    */
  private def invalidated(tree: Tree, edit: Location): Tree = {
    val invalidated = mutable.TreeSet[Changeset.Node]()
    tree.iterator.foreach { node =>
      if (intersect(edit, node)) {
        invalidated += node
        tree -= node
      }
    }
    invalidated
  }

  /** Check if the node location intersects the edit location.
    *
    * @param edit location of the edit
    * @param node the node
    * @return true if the node and edit locations are intersecting
    */
  private def intersect(edit: Location, node: Changeset.Node): Boolean = {
    intersect(edit, node.location)
  }

  /** Check if the node location intersects the edit location.
    *
    * @param edit location of the edit
    * @param node location of the node
    * @return true if the node and edit locations are intersecting
    */
  private def intersect(edit: Location, node: Location): Boolean = {
    inside(node.start, edit) ||
    inside(node.end, edit) ||
    inside(edit.start, node) ||
    inside(edit.end, node)
  }

  /** Check if the character position index is inside the location.
    *
    * @param index the character position
    * @param location the location
    * @return true if the index is inside the location
    */
  private def inside(index: Int, location: Location): Boolean =
    index >= location.start && index <= location.end

  /** Convert [[TextEdit]] to [[Changeset.LocationEdit]] edit in the provided
    * source.
    *
    * @param edit the text edit
    * @param source the source text
    * @return the edit location in the source text
    */
  private def toLocationEdit[A: IndexedSource](
    edit: TextEdit,
    source: A
  ): LocationEdit = {
    LocationEdit(toLocation(edit, source), edit.text.length)
  }

  /** Convert [[TextEdit]] location to [[Location]] in the provided source.
    *
    * @param edit the text edit
    * @param source the source text
    * @return location of the text edit in the source text
    */
  private def toLocation[A: IndexedSource](
    edit: TextEdit,
    source: A
  ): Location = {
    Location(
      IndexedSource[A].toIndex(edit.range.start, source),
      IndexedSource[A].toIndex(edit.range.end, source)
    )
  }

  /** Convert invalidated node to the dataflow dependency type.
    *
    * @param node the invalidated node
    * @return the dataflow dependency type
    */
  private def toDataflowDependencyType(
    node: NodeId
  ): DataflowAnalysis.DependencyInfo.Type.Static =
    DataflowAnalysis.DependencyInfo.Type
      .Static(node.internalId, node.externalId)
}
