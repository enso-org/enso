package org.enso.interpreter.runtime

import org.enso.compiler.Compiler
import org.enso.compiler.context.CompilerContext
import org.enso.editions.LibraryName
import org.enso.pkg.QualifiedName
import org.enso.interpreter.caches.ImportExportCache
import org.enso.interpreter.caches.SuggestionsCache

import java.util.logging.Level

import scala.jdk.OptionConverters.RichOptional

final class SerializationManager(private val context: TruffleCompilerContext) {

  def this(compiler: Compiler) = {
    this(compiler.context.asInstanceOf[TruffleCompilerContext])
  }

  import SerializationManager._

  private val pool = new SerializationPool(context)

  def getPool(): SerializationPool = pool

  // Make sure it is started to avoid races with language shutdown with low job
  // count.
  if (context.isCreateThreadAllowed) {
    pool.prestartAllCoreThreads()
  }

  // === Interface ============================================================

  def deserializeSuggestions(
    libraryName: LibraryName
  ): Option[SuggestionsCache.CachedSuggestions] = {
    if (pool.isWaitingForSerialization(libraryName.toQualifiedName)) {
      pool.abort(libraryName.toQualifiedName)
      None
    } else {
      pool.waitWhileSerializing(libraryName.toQualifiedName)
      val cache = SuggestionsCache.create(libraryName)
      context.loadCache(cache).toScala match {
        case result @ Some(_: SuggestionsCache.CachedSuggestions) =>
          context.logSerializationManager(
            Level.FINE,
            "Restored suggestions for library [{0}].",
            libraryName
          )
          result
        case None =>
          context.logSerializationManager(
            Level.FINE,
            "Unable to load suggestions for library [{0}].",
            libraryName
          )
          None
      }
    }
  }

  def deserializeLibraryBindings(
    libraryName: LibraryName
  ): Option[ImportExportCache.CachedBindings] = {
    if (pool.isWaitingForSerialization(libraryName.toQualifiedName)) {
      pool.abort(libraryName.toQualifiedName)
      None
    } else {
      pool.waitWhileSerializing(libraryName.toQualifiedName)
      val cache = ImportExportCache.create(libraryName)
      context.loadCache(cache).toScala match {
        case result @ Some(_: ImportExportCache.CachedBindings) =>
          context.logSerializationManager(
            Level.FINE,
            "Restored bindings for library [{0}].",
            libraryName
          )
          result
        case _ =>
          context.logSerializationManager(
            Level.FINEST,
            "Unable to load bindings for library [{0}].",
            libraryName
          )
          None
      }

    }
  }

  def shutdown(waitForPendingJobCompletion: Boolean = false): Unit =
    pool.shutdown(waitForPendingJobCompletion)
}

object SerializationManager {

  implicit private class LibraryOps(val libraryName: LibraryName)
      extends AnyVal {
    def toQualifiedName: QualifiedName =
      QualifiedName(List(libraryName.namespace), libraryName.name)
  }

  def apply(context: CompilerContext): SerializationManager = {
    context.asInstanceOf[TruffleCompilerContext].getSerializationManager()
  }
}
