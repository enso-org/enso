package org.enso.interpreter.instrument.execution

import org.enso.interpreter.runtime.Module

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap};

/** Modules need to be indexed for building suggestions DB. */
final class ModuleIndexing {

  object IndexState extends Enumeration {
    val NotIndexed, NeedsIndexing, Indexed = Value
  }

  private val modules: ConcurrentMap[Module, IndexState.Value] =
    new ConcurrentHashMap()

  /** @return true, if module has been indexed. False otherwise.
    */
  def isIndexed(module: Module): Boolean = {
    modules.getOrDefault(module, IndexState.NotIndexed) == IndexState.Indexed;
  }

  /** Marks the module as fully indexed.
    *
    * Prior to indexing the module, it should be set as not index.
    * If during the indexing operation, module is invalidated, it will have to be re-indexed until successful.
    */
  def markAsIndexed(module: Module): Boolean = {
    val v = modules.compute(
      module,
      { case (_, v) =>
        if (v == IndexState.NotIndexed) IndexState.Indexed else v
      }
    )
    v == IndexState.Indexed
  }

  /** Marks the module as requiring indexing. */
  def markIndexAsDirty(module: Module): Unit = {
    modules.compute(module, { case _ => IndexState.NeedsIndexing });
  }

  /** Marks the module as not indexed */
  def markAsNotIndexed(module: Module): Unit = {
    modules.compute(module, { case _ => IndexState.NotIndexed })
  }

}
