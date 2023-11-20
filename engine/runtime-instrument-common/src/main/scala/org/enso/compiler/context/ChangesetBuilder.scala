package org.enso.compiler.context

import com.oracle.truffle.api.source.Source

import java.util.UUID
import org.enso.compiler.core.{
  CompilerError,
  EnsoParser,
  ExternalID,
  IR,
  Identifier
}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Literal
import org.enso.compiler.core.ir.Location
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.text.editing.model.TextEdit
import org.enso.text.editing.{IndexedSource, TextEditor}

import scala.collection.mutable
import scala.util.Using

/** The changeset of a module containing the computed list of invalidated
  * expressions.
  *
  * @param source the module source
  * @param ir the IR node of the module
  * @param simpleUpdate description of a simple editing change (usually in a literal)
  * @param invalidated the list of invalidated expressions
  * @tparam A the source type
  */
case class Changeset[A](
  source: A,
  ir: IR,
  simpleUpdate: Option[SimpleUpdate],
  invalidated: Set[UUID @ExternalID]
)

/** Compute invalidated expressions.
  *
  * @param source the text source
  * @param ir the IR node
  * @tparam A the source type
  */
final class ChangesetBuilder[A: TextEditor: IndexedSource](
  val source: A,
  val ir: IR
) {

  /** Build the changeset containing the nodes invalidated by the edits.
    *
    * @param edits the edits applied to the source
    * @return the computed changeset
    */
  @throws[CompilerError]
  def build(edits: Seq[PendingEdit]): Changeset[A] = {

    val simpleEditOptionFromSetValue: Option[PendingEdit.SetExpressionValue] =
      edits.collect { case edit: PendingEdit.SetExpressionValue =>
        edit
      }.lastOption

    // Try to detect if the text edit is a simple edit
    val simpleEditFromTextEditOption = edits match {
      case Seq(first) => Some(first)
      case Seq(head, realEdit) =>
        val firstAffected = invalidated(Seq(head.edit))
        if (firstAffected.isEmpty) {
          Some(realEdit)
        } else {
          None
        }
      case _ => None
    }

    val simpleEditOption =
      simpleEditOptionFromSetValue.orElse(simpleEditFromTextEditOption)

    val simpleUpdateOption = simpleEditOption
      .filter(e => e.edit.range.start.line == e.edit.range.end.line)
      .map(e => (e, invalidated(Seq(e.edit))))
      .filter(_._2.size == 1)
      .flatMap { case (pending, directlyAffected) =>
        val directlyAffectedId = directlyAffected.head.externalId
        val literals =
          ir.preorder.filter(_.getExternalId == directlyAffectedId)
        val oldIr = literals.head

        def newIR(edit: PendingEdit): Option[Literal] = {
          val value = edit match {
            case pending: PendingEdit.SetExpressionValue => pending.value
            case other: PendingEdit.ApplyEdit            => other.edit.text
          }

          val source = Source.newBuilder("enso", value, null).build
          Using(new EnsoParser) { compiler =>
            compiler
              .generateIRInline(compiler.parse(source.getCharacters()))
              .flatMap(_ match {
                case ir: Literal => Some(ir.setLocation(oldIr.location))
                case _           => None
              })
          }.get
        }

        oldIr match {
          case node: Literal.Number =>
            newIR(pending).map(ir => new SimpleUpdate(node, pending.edit, ir))
          case node: Literal.Text =>
            newIR(pending).map(ir => new SimpleUpdate(node, pending.edit, ir))
          case _ => None
        }
      }

    Changeset(source, ir, simpleUpdateOption, compute(edits.map(_.edit)))
  }

  /** Traverses the IR and returns a list of all IR nodes affected by the edit
    * using the [[DataflowAnalysis]] information.
    *
    * @param edits the text edits
    * @throws CompilerError if the IR is missing DataflowAnalysis metadata
    * @return the set of all IR nodes affected by the edit
    */
  @throws[CompilerError]
  def compute(edits: Seq[TextEdit]): Set[UUID @ExternalID] = {
    val metadata = ir
      .unsafeGetMetadata(
        DataflowAnalysis,
        "Empty dataflow analysis metadata during changeset calculation."
      )

    @scala.annotation.tailrec
    def go(
      queue: mutable.Queue[DataflowAnalysis.DependencyInfo.Type],
      visited: mutable.Set[DataflowAnalysis.DependencyInfo.Type]
    ): Set[UUID @ExternalID] =
      if (queue.isEmpty) visited.flatMap(_.externalId).toSet
      else {
        val elem       = queue.dequeue()
        val transitive = metadata.dependents.get(elem).getOrElse(Set())
        val dynamic = transitive
          .flatMap {
            case DataflowAnalysis.DependencyInfo.Type.Static(int, _) =>
              ChangesetBuilder
                .getExpressionName(ir, int)
                .map(DataflowAnalysis.DependencyInfo.Type.Dynamic(_, None))
            case dyn: DataflowAnalysis.DependencyInfo.Type.Dynamic =>
              Some(dyn)
            case _ =>
              None
          }
          .flatMap(metadata.dependents.get)
          .flatten
        val combined = transitive.union(dynamic)

        go(
          queue ++= combined.diff(visited),
          visited ++= combined
        )
      }

    val nodeIds = invalidated(edits)
    val direct  = nodeIds.flatMap(ChangesetBuilder.toDataflowDependencyTypes)
    val transitive =
      go(
        mutable.Queue().addAll(direct),
        mutable.Set()
      )
    direct.flatMap(_.externalId) ++ transitive
  }

  /** Traverses the IR and returns a list of the most specific (the innermost)
    * IR nodes directly affected by the edit by comparing the source locations.
    *
    * @param edits the text edits
    * @return the set of IR nodes directly affected by the edit
    */
  def invalidated(edits: Seq[TextEdit]): Set[ChangesetBuilder.NodeId] = {
    @scala.annotation.tailrec
    def go(
      tree: ChangesetBuilder.Tree,
      source: A,
      edits: mutable.Queue[TextEdit],
      ids: mutable.Set[ChangesetBuilder.NodeId]
    ): Set[ChangesetBuilder.NodeId] = {
      if (edits.isEmpty) ids.toSet
      else {
        val edit         = edits.dequeue()
        val locationEdit = ChangesetBuilder.toLocationEdit(edit, source)
        var invalidatedSet =
          ChangesetBuilder.invalidated(tree, locationEdit.location, true)
        if (invalidatedSet.isEmpty) {
          invalidatedSet =
            ChangesetBuilder.invalidated(tree, locationEdit.location, false)
        }
        val newTree   = ChangesetBuilder.updateLocations(tree, locationEdit)
        val newSource = TextEditor[A].edit(source, edit)
        go(newTree, newSource, edits, ids ++= invalidatedSet.map(_.id))
      }
    }
    val tree = ChangesetBuilder.buildTree(ir)
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

object ChangesetBuilder {

  type Symbol = String

  /** An identifier of IR node.
    *
    * @param internalId internal IR id
    * @param externalId external IR id
    * @param name optional node name
    */
  case class NodeId(
    internalId: UUID @Identifier,
    externalId: Option[UUID @ExternalID],
    name: Option[Symbol]
  )

  object NodeId {

    /** Create a [[NodeId]] identifier from [[IR]].
      *
      * @param ir the source IR
      * @return the identifier
      */
    def apply(ir: IR): NodeId =
      new NodeId(ir.getId, ir.getExternalId, getName(ir))

    implicit val ordering: Ordering[NodeId] = (x: NodeId, y: NodeId) => {
      val cmpInternal =
        Ordering[UUID @Identifier].compare(x.internalId, y.internalId)
      if (cmpInternal == 0) {
        Ordering[Option[UUID @ExternalID]].compare(x.externalId, y.externalId)
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
  private case class Node(id: NodeId, location: Location, leaf: Boolean) {

    /** Shift the node location.
      *
      * @param offset the offset relative to the previous node location
      * @return the node with a new location
      */
    def shift(offset: Int): Node = {
      val newLocation = new Location(
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
      ir.location.map(loc => Node(NodeId(ir), loc.location, true))

    /** Create an artificial node with fixed [[NodeId]]. It is used to select
      * nodes by location in the tree.
      *
      * @param location the location of the node
      * @return a select node
      */
    def select(location: Location): Node =
      new Node(
        NodeId(UUID.nameUUIDFromBytes(Array()), None, None),
        location,
        false
      )

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

  /** Get the IR name if available. */
  private def getName(ir: IR): Option[String] = ir match {
    case name: Name => Some(name.name)
    case _          => None
  }

  /** Build an internal representation of the [[IR]].
    *
    * @param ir the source IR
    * @return the tree representation of the IR
    */
  private def buildTree(ir: IR): Tree = {
    def depthFirstSearch(currentIr: IR, acc: Tree): Unit = {
      if (currentIr.children.isEmpty) {
        Node.fromIr(currentIr).foreach(acc.add)
      } else {
        val hasImportantId = currentIr.getExternalId.nonEmpty
        if (hasImportantId) {
          val collectChildrenIntervals = new Tree()
          currentIr.children.map(depthFirstSearch(_, collectChildrenIntervals))

          def fillGapsInChildrenNodesWithNonLeafNodes(
            previousPosition: Int,
            nextNode: Node
          ): Int = {
            if (previousPosition < nextNode.location.start) {
              val nodeBetweenPreviousPositionAndNextNode =
                Node(
                  NodeId(currentIr),
                  new Location(previousPosition, nextNode.location.start),
                  false
                )
              acc += nodeBetweenPreviousPositionAndNextNode
            }
            acc += nextNode
            nextNode.location.end
          }
          val beginOfNonLeafIr = currentIr.location.get.location.start
          val endOfNonLeafIr   = currentIr.location.get.location.end
          val lastCoveredPosition =
            collectChildrenIntervals.foldLeft(beginOfNonLeafIr)(
              fillGapsInChildrenNodesWithNonLeafNodes
            )
          val hasRemainingTextAfterLastChild =
            lastCoveredPosition < endOfNonLeafIr
          if (hasRemainingTextAfterLastChild) {
            val nodeAfterLastChild = Node(
              NodeId(currentIr),
              new Location(lastCoveredPosition, endOfNonLeafIr),
              false
            )
            acc += nodeAfterLastChild
          }
        } else {
          currentIr.children.map(depthFirstSearch(_, acc))
        }
      }
    }
    val collectNodes = new Tree()
    depthFirstSearch(ir, collectNodes)
    collectNodes
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
  private def invalidated(
    tree: Tree,
    edit: Location,
    onlyLeafs: Boolean
  ): Tree = {
    val invalidated = mutable.TreeSet[ChangesetBuilder.Node]()
    tree.iterator.foreach { node =>
      if (!onlyLeafs || node.leaf) {
        if (intersect(edit, node)) {
          invalidated += node
          tree -= node
        }
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
  private def intersect(
    edit: Location,
    node: ChangesetBuilder.Node
  ): Boolean = {
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

  /** Convert [[TextEdit]] to [[ChangesetBuilder.LocationEdit]] edit in the provided
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
    new Location(
      IndexedSource[A].toIndex(edit.range.start, source),
      IndexedSource[A].toIndex(edit.range.end, source)
    )
  }

  /** Convert invalidated node to the dataflow dependency type.
    *
    * @param node the invalidated node
    * @return the dataflow dependency type
    */
  private def toDataflowDependencyTypes(
    node: NodeId
  ): Seq[DataflowAnalysis.DependencyInfo.Type] = {
    val static = DataflowAnalysis.DependencyInfo.Type
      .Static(node.internalId, node.externalId)
    val dynamic = node.name.map { name =>
      DataflowAnalysis.DependencyInfo.Type.Dynamic(name, node.externalId)
    }
    static +: dynamic.toSeq
  }

  /** Get expression name by the given id.
    *
    * @param ir the IR tree
    * @param id the node identifier
    * @return the node name
    */
  private def getExpressionName(ir: IR, id: UUID @Identifier): Option[String] =
    ir.preorder.find(_.getId == id).collect {
      case name: Name =>
        name.name
      case method: definition.Method =>
        method.methodName.name
    }

}
