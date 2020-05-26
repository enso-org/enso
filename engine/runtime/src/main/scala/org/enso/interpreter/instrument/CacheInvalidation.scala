package org.enso.interpreter.instrument

import java.util.UUID

import org.enso.polyglot.runtime.Runtime.Api

import scala.jdk.CollectionConverters._

/**
  * Base trait for cache invalidation instructions.
  */
sealed trait CacheInvalidation

object CacheInvalidation {

  /**
    * Instruction to invalidate all cache entries.
    */
  case object InvalidateAll extends CacheInvalidation

  /**
    * Instruction to invalidate provided cache keys.
    *
    * @param keys a list of keys that should be invalidated.
    */
  case class InvalidateKeys(keys: Iterable[UUID]) extends CacheInvalidation

  /**
    * Instruction to invalidate stale entries from the cache.
    *
    * @param scope all ids of the source.
    */
  case class InvalidateStale(scope: Iterable[UUID]) extends CacheInvalidation

  /**
    * Construct invalidation instruction from [[Api.InvalidatedExpressions]].
    *
    * @param expr Api invalidated expressions.
    * @return an invalidation instruction.
    */
  def apply(expr: Api.InvalidatedExpressions): CacheInvalidation =
    expr match {
      case Api.InvalidatedExpressions.All() =>
        InvalidateAll
      case Api.InvalidatedExpressions.Expressions(ids) =>
        InvalidateKeys(ids)
    }

  /**
    * Run cache invalidation.
    *
    * @param stack the stack which cache should be invalidated.
    * @param rules the list of invalidation instruction.
    */
  def run(
    stack: Iterable[InstrumentFrame],
    rules: Iterable[CacheInvalidation]
  ): Unit = {
    stack.headOption.map(_.cache).foreach { cache =>
      rules.foreach(run(cache, _))
    }
  }

  /**
    * Run cache invalidation.
    *
    * @param cache the cache which should be invalidated.
    * @param rule the invalidation instruction.
    */
  def run(cache: RuntimeCache, rule: CacheInvalidation): Unit =
    rule match {
      case InvalidateAll =>
        cache.clear()
      case InvalidateKeys(keys) =>
        keys.foreach(cache.remove)
      case InvalidateStale(scope) =>
        val staleKeys = cache.getKeys.asScala.diff(scope.toSet)
        staleKeys.foreach(cache.remove)
    }
}
