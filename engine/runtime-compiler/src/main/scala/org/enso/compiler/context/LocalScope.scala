package org.enso.compiler.context

import org.enso.compiler.pass.analyse.AliasAnalysis.Graph
import org.enso.compiler.pass.analyse.AliasAnalysis.Graph.{
  Id,
  Occurrence,
  Scope => AliasScope
}
import org.enso.compiler.pass.analyse.{AliasAnalysis, DataflowAnalysis}

import scala.jdk.CollectionConverters._

/** A representation of an Enso local scope.
  *
  * Enso local scopes may be arbitrarily nested, and are used to maintain a
  * mapping between the interpreter's concept of stack frames and the guest
  * language's concept of stack frames.
  *
  * Please note that `flattenToParent` is a _hack_ and will be removed once we
  * have demand analysis.
  *
  * @param parentScope the parent local scope for this scope, if it exists
  * @param aliasingGraph the graph containing aliasing information for the tree
  *                      of scopes within which this local scope exists
  * @param scope the particular scope in `aliasingGraph` represented by this
  *              [[LocalScope]].
  * @param dataflowInfo information on the dataflow analysis for this scope
  * @param flattenToParent whether or not the frame should be flattened into its
  *                        parent
  * @param parentFrameSlotIdxs Mapping of occurence identifiers to frame slot indexes
  *       from the whole parent hierarchy, i.e., this parameter should contain all the
  *       indexes for all the parents.
  */
class LocalScope(
  final val parentScope: Option[LocalScope],
  final val aliasingGraph: AliasAnalysis.Graph,
  final val scope: AliasAnalysis.Graph.Scope,
  final val dataflowInfo: DataflowAnalysis.Metadata,
  final val flattenToParent: Boolean                  = false,
  private val parentFrameSlotIdxs: Map[Graph.Id, Int] = Map()
) {
  private lazy val localFrameSlotIdxs: Map[Graph.Id, Int] =
    gatherLocalFrameSlotIdxs()

  /** All frame slot indexes, including local and all the parents.
    * Useful for quick searching for [[FramePointer]] of parent scopes.
    */
  private lazy val allFrameSlotIdxs: Map[Graph.Id, Int] =
    parentFrameSlotIdxs ++ localFrameSlotIdxs

  /** Creates a new child with a new aliasing scope.
    *
    * @return a child of this scope
    */
  def createChild(): LocalScope = createChild(scope.addChild())

  /** Creates a child using a known aliasing scope.
    *
    * @param childScope the known child
    * @param flattenToParent whether or not the child scope should be flattened
    *                        with its parent
    * @return a child of this scope
    */
  def createChild(
    childScope: AliasScope,
    flattenToParent: Boolean = false
  ): LocalScope = {
    new LocalScope(
      Some(this),
      aliasingGraph,
      childScope,
      dataflowInfo,
      flattenToParent,
      allFrameSlotIdxs
    )
  }

  /** Get a frame slot index for a given identifier.
    *
    * The identifier must be present in the local scope.
    *
    * @param id the identifier of a variable definition occurrence from alias
    *           analysis.
    * @return the frame slot index for `id`.
    */
  def getVarSlotIdx(id: Graph.Id): Int = {
    assert(localFrameSlotIdxs.contains(id))
    localFrameSlotIdxs(id)
  }

  /** Obtains the frame pointer for a given identifier from the current scope, or from
    * any parent scopes.
    *
    * @param id the identifier of a variable usage occurrence from alias
    *           analysis
    * @return the frame pointer for `id`, if it exists
    */
  def getFramePointer(id: Graph.Id): Option[FramePointer] = {
    aliasingGraph
      .defLinkFor(id)
      .flatMap { link =>
        val slotIdx = allFrameSlotIdxs.get(link.target)
        slotIdx.map(
          new FramePointer(
            if (flattenToParent) link.scopeCount - 1 else link.scopeCount,
            _
          )
        )
      }
  }

  /** Collects all the bindings in the current stack of scopes, accounting for
    * shadowing.
    *
    * @return a map of binding names to their associated frame pointers
    */
  def flattenBindings: java.util.Map[String, FramePointer] =
    flattenBindingsWithLevel(0).asJava

  /** Gather local variables from the alias scope information.
    * Does not include any variables from the parent scopes.
    * @return Mapping of local variable identifiers to their
    *         indexes in the frame. Takes into account all the
    *         internal slots, that are prepended to every frame.
    */
  private def gatherLocalFrameSlotIdxs(): Map[Id, Int] = {
    scope.allDefinitions.zipWithIndex.map { case (definition, i) =>
      definition.id -> (i + LocalScope.internalSlotsSize)
    }.toMap
  }

  /** Flatten bindings from a given set of levels, accounting for shadowing.
    *
    * @param level the level to which to flatten
    * @return a map of binding names to their associated frame pointers
    */
  private def flattenBindingsWithLevel(
    level: Int
  ): Map[Graph.Symbol, FramePointer] = {
    var parentResult: Map[Graph.Symbol, FramePointer] = parentScope
      .flatMap(scope => Some(scope.flattenBindingsWithLevel(level + 1)))
      .getOrElse(Map())

    scope.occurrences.foreach {
      case x: Occurrence.Def =>
        parentResult += x.symbol -> new FramePointer(
          level,
          allFrameSlotIdxs(x.id)
        )
      case _ =>
    }
    parentResult
  }

  override def toString: String = {
    s"LocalScope(${allFrameSlotIdxs.keySet})"
  }
}
object LocalScope {

  /** Constructs a local scope for an [[EnsoRootNode]].
    *
    * @return a defaulted local scope
    */
  def root: LocalScope = {
    val graph = new AliasAnalysis.Graph
    new LocalScope(
      None,
      graph,
      graph.rootScope,
      DataflowAnalysis.DependencyInfo()
    )
  }

  /** Internal slots are prepended at the beginning of every [[FrameDescriptor]].
    * Every tuple of the list denotes frame slot kind and its name.
    * Note that `info` for a frame slot is not used by Enso.
    */
  def monadicStateSlotName: String   = "<<monadic_state>>"
  private def internalSlotsSize: Int = 1

}
