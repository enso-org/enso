package org.enso.compiler.context

import org.enso.compiler.pass.analyse.FrameAnalysisMeta
import org.enso.compiler.pass.analyse.FramePointer
import org.enso.compiler.pass.analyse.FrameVariableNames
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.compiler.pass.analyse.alias.graph.{
  GraphOccurrence,
  GraphBuilder,
  Graph => AliasGraph
}

import scala.jdk.CollectionConverters._
import java.util.function.BiFunction

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
  final val aliasingGraph: () => AliasGraph,
  final private val scopeProvider: () => AliasGraph.Scope,
  final private val dataflowInfoProvider: () => DataflowAnalysis.Metadata,
  final private val symbolsProvider: () => FrameAnalysisMeta     = null,
  final val flattenToParent: Boolean                             = false,
  private val parentFrameSlotIdxs: () => Map[AliasGraph.Id, Int] = () => Map()
) {
  lazy val scope: AliasGraph.Scope                 = scopeProvider()
  lazy val dataflowInfo: DataflowAnalysis.Metadata = dataflowInfoProvider()

  /** Computes allSymbols needed by this scope. Either the value is obtained
    * from symbolsProvider or (as a fallback) a computation from aliasingGraph
    * is performed - the latter situation is logged, when log is not null.
    *
    * @param where human "friendly" identification of the caller
    * @param log function to log or null if no logging is needed
    */
  def allSymbols(
    where: String,
    log: BiFunction[String, Array[Object], Void]
  ): java.util.List[String] = {
    def symbols(): java.util.List[String] = {
      val r = scope.allDefinitions.map(_.symbol)
      r.asJava
    }
    val meta = if (symbolsProvider == null) null else symbolsProvider()
    if (meta.isInstanceOf[FrameVariableNames]) {
      val cached = meta.asInstanceOf[FrameVariableNames].variableNames()
      if (log != null) {
        ContextUtils.assertSame(where, cached, () => symbols())
      }
      cached
    } else {
      val result = symbols()
      if (log != null) {
        log(
          "Scope computed from AliasAnalysis at {0} = {1}",
          Array(where, result)
        )
      }
      result
    }
  }

  private lazy val localFrameSlotIdxs: Map[AliasGraph.Id, Int] =
    gatherLocalFrameSlotIdxs()

  /** All frame slot indexes, including local and all the parents.
    * Useful for quick searching for [[FramePointer]] of parent scopes.
    */
  private lazy val allFrameSlotIdxs: Map[AliasGraph.Id, Int] =
    parentFrameSlotIdxs() ++ localFrameSlotIdxs

  /** Creates a new child with a new aliasing scope.
    *
    * @return a child of this scope
    */
  def createChild(): LocalScope = createChild(() => {
    GraphBuilder.create(null, scope).addChild().toScope()
  })

  /** Creates a child using a known aliasing scope.
    *
    * @param childScope the known child
    * @param flattenToParent whether or not the child scope should be flattened
    *                        with its parent
    * @return a child of this scope
    */
  def createChild(
    childScope: () => AliasGraph.Scope,
    flattenToParent: Boolean                  = false,
    symbolsProvider: () => FrameVariableNames = null
  ): LocalScope = {
    val sp = if (flattenToParent) {
      org.enso.common.Asserts.assertInJvm(symbolsProvider == null)
      this.symbolsProvider
    } else {
      symbolsProvider
    }
    new LocalScope(
      Some(this),
      aliasingGraph,
      childScope,
      () => dataflowInfo,
      sp,
      flattenToParent,
      () => allFrameSlotIdxs
    )
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
  private def gatherLocalFrameSlotIdxs(): Map[AliasGraph.Id, Int] = {
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
  ): Map[AliasGraph.Symbol, FramePointer] = {
    var parentResult: Map[AliasGraph.Symbol, FramePointer] = parentScope
      .flatMap(scope => Some(scope.flattenBindingsWithLevel(level + 1)))
      .getOrElse(Map())

    scope.occurrences.foreach {
      case (id, x: GraphOccurrence.Def) =>
        parentResult += x.symbol -> new FramePointer(
          level,
          allFrameSlotIdxs(id)
        )
      case _ =>
    }
    parentResult
  }

  override def toString: String = {
    s"LocalScope(flattenToParent = ${flattenToParent}, allFrameSlotIdxs = ${allFrameSlotIdxs.keySet})"
  }
}
object LocalScope {

  /** Empty and immutable singleton scope.
    */
  val empty: LocalScope = {
    val graph = new AliasGraph
    graph.freeze()
    val info               = DataflowAnalysis.DependencyInfo()
    val emptyVariableNames = FrameVariableNames.create(List())
    new LocalScope(
      None,
      () => graph,
      () => graph.rootScope,
      () => info,
      () => emptyVariableNames
    )
  }

  /** Constructs a new local scope for an [[EnsoRootNode]] that can then be modified.
    *
    * @return a new empty scope ready for additional modifications.
    */
  def createEmpty: LocalScope = {
    val graph = new AliasGraph
    val info  = DataflowAnalysis.DependencyInfo()
    new LocalScope(
      None,
      () => graph,
      () => graph.rootScope,
      () => info
    )
  }

  /** Internal slots are prepended at the beginning of every [[FrameDescriptor]].
    * Every tuple of the list denotes frame slot kind and its name.
    * Note that `info` for a frame slot is not used by Enso.
    */
  def monadicStateSlotName: String = "<<monadic_state>>"
  def internalSlotsSize: Int       = 1

}
