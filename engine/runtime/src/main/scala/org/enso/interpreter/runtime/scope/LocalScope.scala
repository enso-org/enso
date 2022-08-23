package org.enso.interpreter.runtime.scope

import com.oracle.truffle.api.frame.{FrameDescriptor, FrameSlotKind}
import org.enso.compiler.pass.analyse.AliasAnalysis.Graph
import org.enso.compiler.pass.analyse.AliasAnalysis.Graph.{Occurrence, Scope => AliasScope}
import org.enso.compiler.pass.analyse.{AliasAnalysis, DataflowAnalysis}

import scala.collection.mutable
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
  * @param frameSlotIdxs a mapping from symbol definition identifiers to slot indexes in
  *                      the Enso frame.
  */
class LocalScope(
  final val parentScope: Option[LocalScope],
  final val aliasingGraph: AliasAnalysis.Graph,
  final val scope: AliasAnalysis.Graph.Scope,
  final val dataflowInfo: DataflowAnalysis.Metadata,
  final val flattenToParent: Boolean                     = false,
  final val frameSlotIdxs: mutable.Map[Graph.Id, Int]    = mutable.Map()
) {

  private val frameDescrBuilder: FrameDescriptor.Builder = FrameDescriptor.newBuilder()
  private var frameDescriptor: Option[FrameDescriptor] = None
  val monadicStateSlotIdx: Int = frameDescrBuilder.addSlot(FrameSlotKind.Object, "<<monadic_state>>", null)

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
      frameSlotIdxs
    )
  }

  /** Creates a frame slot for a given identifier and returns index into that
    * slot.
    *
    * Also invalidates the previously built [[FrameDescriptor]] with
    * [[buildFrameDescriptor()]].
    *
    * @param id the identifier of a variable definition occurrence from alias
    *           analysis
    * @return a new frame slot index for `id`
    */
  def createVarSlotIdx(id: Graph.Id): Int = {
    val name = aliasingGraph.idToSymbol(id)
    val slotIdx = frameDescrBuilder.addSlot(FrameSlotKind.Illegal, name, null)
    if (frameDescriptor.isDefined) {
      frameDescriptor = None
    }
    frameSlotIdxs(id) = slotIdx
    slotIdx
  }

  /**
   * Builds the [[FrameDescriptor]] from all the slots there have been added via
   * [[createVarSlotIdx]]. May be called multiple times.
   *
   * Each subsequent call creates a new [[FrameDescriptor]] that is a _superset_ of
   * the previous [[FrameDescriptor]] built with this method.
   * A [[FrameDescriptor]] `fd1` is a _superset_ of [[FrameDescriptor]] `fd2` iff
   * the set of all the slot names in `fd1` is a superset of the set of all the slot
   * names in `fd2`.
   */
  def buildFrameDescriptor(): Unit = {
    frameDescriptor = Some(frameDescrBuilder.build())
  }

  /**
   * Returns the previously built [[FrameDescriptor]] via [[buildFrameDescriptor()]]
   *
   * @return Previously built frame descriptor.
   */
  def getFrameDescriptor: FrameDescriptor = {
    assert(frameDescriptor.nonEmpty)
    frameDescriptor.get
  }

  /** Obtains the frame pointer for a given identifier.
    *
    * @param id the identifier of a variable usage occurrence from alias
    *           analysis
    * @return the frame pointer for `id`, if it exists
    */
  def getFramePointer(id: Graph.Id): Option[FramePointer] = {
    aliasingGraph.defLinkFor(id).flatMap { link =>
      val slotIdx = frameSlotIdxs.get(link.target)
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
          frameSlotIdxs(x.id)
        )
      case _ =>
    }
    parentResult
  }

  override def toString: String = {
    frameDescriptor match {
      case None => "Not built"
      case Some(fd: FrameDescriptor) => fd.toString
    }
  }
}
object LocalScope {

  /** Constructs a local scope for an
    * [[org.enso.interpreter.node.EnsoRootNode]].
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
}
