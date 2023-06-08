package org.enso.interpreter.instrument

import java.util.UUID

import org.enso.compiler.pass.analyse.CachePreferenceAnalysis
import org.enso.polyglot.runtime.Runtime.Api

import scala.jdk.CollectionConverters._

/** Cache invalidation instruction.
  *
  * An instruction describes the stack `elements` selected for invalidation, the
  * invalidation `command` itself, and the extra set of `indexes` that should also
  * be cleared with an invalidation `command`.
  *
  * @param elements the cache of which stack elements to invalidate
  * @param command the invalidation command
  * @param indexes the extra indexes to invalidate
  */
case class CacheInvalidation(
  elements: CacheInvalidation.StackSelector,
  command: CacheInvalidation.Command,
  indexes: Set[CacheInvalidation.IndexSelector]
)

object CacheInvalidation {

  /** Selector of the cache index. */
  sealed trait IndexSelector
  object IndexSelector {

    /** Invalidate value from all indexes. */
    case object All extends IndexSelector

    /** Invalidate the types index. */
    case object Types extends IndexSelector

    /** Invalidate the calls index. */
    case object Calls extends IndexSelector

    /** Invalidate the weights index. */
    case object Weights extends IndexSelector
  }

  /** Base trait for cache invalidation commands. Commands describe how the
    * state of the cache is changed.
    */
  sealed trait Command
  object Command {

    /** A command to invalidate all cache entries. */
    case object InvalidateAll extends Command

    /** A command to invalidate provided cache keys.
      *
      * @param keys a list of keys that should be invalidated
      */
    case class InvalidateKeys(keys: Iterable[UUID]) extends Command

    /** A command to invalidate stale entries from the cache.
      *
      * @param scope all ids of the source
      */
    case class InvalidateStale(scope: Iterable[UUID]) extends Command

    /** A command to set the cache metadata form the compiler pass.
      *
      * @param metadata the cache metadata
      */
    case class SetMetadata(metadata: CachePreferenceAnalysis.Metadata)
        extends Command

    /** Create an invalidation command from [[Api.InvalidatedExpressions]].
      *
      * @param expressions invalidated expressions
      * @return an invalidation command
      */
    def apply(expressions: Api.InvalidatedExpressions): Command =
      expressions match {
        case Api.InvalidatedExpressions.All() =>
          InvalidateAll
        case Api.InvalidatedExpressions.Expressions(ids) =>
          InvalidateKeys(ids)
      }
  }

  /** Base trait for selecting stack elements. */
  sealed trait StackSelector
  object StackSelector {

    /** Select all stack elements. */
    case object All extends StackSelector

    /** Select top stack element. */
    case object Top extends StackSelector
  }

  /** Create an invalidation instruction using a stack selector and an
    * invalidation command.
    *
    * @param elements the stack elements selector
    * @param command the invalidation command
    */
  def apply(
    elements: StackSelector,
    command: Command
  ): CacheInvalidation =
    new CacheInvalidation(elements, command, Set())

  /** Invalidate all caches in the provided stack.
    *
    * @param stack the stack to invalidate
    */
  def invalidateAll(stack: Iterable[InstrumentFrame]): Unit =
    run(stack, CacheInvalidation(StackSelector.All, Command.InvalidateAll))

  /** Run a sequence of invalidation instructions on an execution stack.
    *
    * @param stack the runtime stack
    * @param instructions the list of cache invalidation instructions
    */
  def runAll(
    stack: Iterable[InstrumentFrame],
    instructions: Iterable[CacheInvalidation]
  ): Unit =
    instructions.foreach(run(stack, _))

  /** Run a sequence of invalidation instructions on all visualisations.
    *
    * @param visualisations the list of available visualisations
    * @param instructions the list of cache invalidation instructions
    */
  def runAllVisualisations(
    visualisations: Iterable[Visualisation],
    instructions: Iterable[CacheInvalidation]
  ): Unit =
    instructions.foreach { instruction =>
      runVisualisations(
        visualisations,
        instruction.command,
        instruction.indexes
      )
    }

  /** Run cache invalidation of a multiple visualisations
    *
    * @param visualisations visualisations cache should be invalidated
    * @param command the invalidation instruction
    * @param indexes the list of indexes to invalidate
    */
  def runVisualisations(
    visualisations: Iterable[Visualisation],
    command: Command,
    indexes: Set[IndexSelector] = Set()
  ): Unit =
    visualisations.foreach { visualisation =>
      run(visualisation.cache, command, indexes)
    }

  /** Run a cache invalidation instruction on an execution stack.
    *
    * @param stack the runtime stack
    * @param instruction the invalidation instruction
    */
  def run(
    stack: Iterable[InstrumentFrame],
    instruction: CacheInvalidation
  ): Unit = {
    val frames = instruction.elements match {
      case StackSelector.All => stack
      case StackSelector.Top => stack.headOption.toSeq
    }
    run(frames, instruction.command, instruction.indexes)
  }

  /** Run cache invalidation of a multiple instrument frames.
    *
    * @param frames stack elements which cache should be invalidated
    * @param command the invalidation instruction
    * @param indexes the list of indexes to invalidate
    */
  private def run(
    frames: Iterable[InstrumentFrame],
    command: Command,
    indexes: Set[IndexSelector]
  ): Unit = {
    frames.foreach(frame => run(frame.cache, frame.syncState, command, indexes))
  }

  /** Run cache invalidation of a single instrument frame.
    *
    * @param cache the cache to invalidate
    * @param command the invalidation instruction
    * @param indexes the list of indexes to invalidate
    */
  private def run(
    cache: RuntimeCache,
    command: Command,
    indexes: Set[IndexSelector]
  ): Unit =
    command match {
      case Command.InvalidateAll =>
        cache.clear()
        indexes.foreach(clearIndex(_, cache))
      case Command.InvalidateKeys(keys) =>
        keys.foreach { key =>
          cache.remove(key)
          indexes.foreach(clearIndexKey(key, _, cache))
        }
      case Command.InvalidateStale(scope) =>
        val staleKeys = cache.getKeys.asScala.diff(scope.toSet)
        staleKeys.foreach { key =>
          cache.remove(key)
          indexes.foreach(clearIndexKey(key, _, cache))
        }
      case Command.SetMetadata(metadata) =>
        cache.setWeights(metadata.asJavaWeights)
    }

  /** Run cache invalidation of a single instrument frame.
    *
    * @param cache the cache to invalidate
    * @param syncState the synchronization state of runtime updates
    * @param command the invalidation instruction
    * @param indexes the list of indexes to invalidate
    */
  private def run(
    cache: RuntimeCache,
    syncState: UpdatesSynchronizationState,
    command: Command,
    indexes: Set[IndexSelector]
  ): Unit =
    command match {
      case Command.InvalidateAll =>
        cache.clear()
        indexes.foreach(clearIndex(_, cache))
      case Command.InvalidateKeys(keys) =>
        keys.foreach { key =>
          cache.remove(key)
          indexes.foreach(clearIndexKey(key, _, cache))
        }
      case Command.InvalidateStale(scope) =>
        val staleKeys = cache.getKeys.asScala.diff(scope.toSet)
        staleKeys.foreach { key =>
          cache.remove(key)
          indexes.foreach(clearIndexKey(key, _, cache))
          syncState.invalidate(key)
        }
      case Command.SetMetadata(metadata) =>
        cache.setWeights(metadata.asJavaWeights)
    }

  /** Clear the selected index.
    *
    * @param selector the selected index
    * @param cache the cache to invalidate
    */
  private def clearIndex(selector: IndexSelector, cache: RuntimeCache): Unit =
    selector match {
      case IndexSelector.All =>
        cache.clearTypes()
        cache.clearWeights()
        cache.clearCalls()
      case IndexSelector.Weights =>
        cache.clearWeights()
      case IndexSelector.Types =>
        cache.clearTypes()
      case IndexSelector.Calls =>
        cache.clearCalls()
    }

  /** Clear the key in the selected index.
    *
    * @param key the index key
    * @param selector the selected index
    * @param cache the cache to invalidate
    */
  private def clearIndexKey(
    key: UUID,
    selector: IndexSelector,
    cache: RuntimeCache
  ): Unit =
    selector match {
      case IndexSelector.All =>
        cache.removeType(key)
        cache.removeWeight(key)
        cache.removeCall(key)
      case IndexSelector.Weights =>
        cache.removeWeight(key)
      case IndexSelector.Types =>
        cache.removeType(key)
      case IndexSelector.Calls =>
        cache.removeCall(key)
    }

}
