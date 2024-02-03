package org.enso.interpreter.runtime

import com.oracle.truffle.api.source.Source
import org.enso.compiler.Compiler
import org.enso.compiler.core.ir.{Module => IRModule}
import org.enso.compiler.context.{ExportsBuilder, ExportsMap, SuggestionBuilder}
import org.enso.compiler.context.CompilerContext
import org.enso.compiler.context.CompilerContext.Module
import org.enso.editions.LibraryName
import org.enso.pkg.QualifiedName
import org.enso.polyglot.Suggestion
import org.enso.polyglot.CompilationStage
import org.enso.interpreter.caches.Cache
import org.enso.interpreter.caches.ImportExportCache
import org.enso.interpreter.caches.ModuleCache
import org.enso.interpreter.caches.SuggestionsCache

import java.io.NotSerializableException
import java.util
import java.util.concurrent.Callable
import java.util.logging.Level

import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._

final class SerializationManager(private val context: TruffleCompilerContext) {

  def this(compiler: Compiler) = {
    this(compiler.context.asInstanceOf[TruffleCompilerContext])
  }

  import SerializationManager._

  /** The debug logging level. */
  private val debugLogLevel = Level.FINE

  private val pool = new SerializationPool(context)

  def getPool(): SerializationPool = pool

  // Make sure it is started to avoid races with language shutdown with low job
  // count.
  if (context.isCreateThreadAllowed) {
    pool.prestartAllCoreThreads()
  }

  // === Interface ============================================================

  def doSerializeLibrary(
    compiler: Compiler,
    libraryName: LibraryName,
    useGlobalCacheLocations: Boolean
  ): Callable[Boolean] = () => {
    pool.waitWhileSerializing(libraryName.toQualifiedName)

    context.logSerializationManager(
      debugLogLevel,
      "Running serialization for bindings [{0}].",
      libraryName
    )
    pool.startSerializing(libraryName.toQualifiedName)
    val bindingsCache = new ImportExportCache.CachedBindings(
      libraryName,
      new ImportExportCache.MapToBindings(
        context
          .getPackageRepository()
          .getModulesForLibrary(libraryName)
          .map { module =>
            val ir = module.getIr
            (
              module.getName,
              ir
            )
          }
          .toMap
          .asJava
      ),
      context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(_.listSourcesJava())
    )
    try {
      val result =
        try {
          val cache = ImportExportCache.create(libraryName)
          val file = context.saveCache(
            cache,
            bindingsCache,
            useGlobalCacheLocations
          )
          file.isPresent
        } catch {
          case e: NotSerializableException =>
            context.logSerializationManager(
              Level.SEVERE,
              s"Could not serialize bindings [$libraryName].",
              e
            )
            throw e
          case e: Throwable =>
            context.logSerializationManager(
              Level.SEVERE,
              s"Serialization of bindings `$libraryName` failed: ${e.getMessage}`",
              e
            )
            throw e
        }

      doSerializeLibrarySuggestions(
        compiler,
        libraryName,
        useGlobalCacheLocations
      )

      result
    } finally {
      pool.finishSerializing(libraryName.toQualifiedName)
    }
  }

  private def doSerializeLibrarySuggestions(
    compiler: Compiler,
    libraryName: LibraryName,
    useGlobalCacheLocations: Boolean
  ): Boolean = {
    val exportsBuilder = new ExportsBuilder
    val exportsMap     = new ExportsMap
    val suggestions    = new util.ArrayList[Suggestion]()

    try {
      val libraryModules =
        context.getPackageRepository().getModulesForLibrary(libraryName)
      libraryModules
        .flatMap { module =>
          val suggestions = SuggestionBuilder(module, compiler)
            .build(module.getName, module.getIr)
            .toVector
            .filter(Suggestion.isGlobal)
          val exports = exportsBuilder.build(module.getName, module.getIr)
          exportsMap.addAll(module.getName, exports)
          suggestions
        }
        .map { suggestion =>
          val reexport = exportsMap.get(suggestion).map(_.toString)
          suggestion.withReexport(reexport)
        }
        .foreach(suggestions.add)
      val cachedSuggestions =
        new SuggestionsCache.CachedSuggestions(
          libraryName,
          new SuggestionsCache.Suggestions(suggestions),
          context
            .getPackageRepository()
            .getPackageForLibraryJava(libraryName)
            .map(_.listSourcesJava())
        )
      val cache = SuggestionsCache.create(libraryName)
      val file = context.saveCache(
        cache,
        cachedSuggestions,
        useGlobalCacheLocations
      )
      file.isPresent
    } catch {
      case e: NotSerializableException =>
        context.logSerializationManager(
          Level.SEVERE,
          s"Could not serialize suggestions [$libraryName].",
          e
        )
        throw e
      case e: Throwable =>
        context.logSerializationManager(
          Level.SEVERE,
          s"Serialization of suggestions `$libraryName` failed: ${e.getMessage}`",
          e
        )
        throw e
    }
  }

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

  /** Deserializes the requested module from the cache if possible.
    *
    * If the requested module is currently being serialized it will wait for
    * completion before loading. If the module is queued for serialization it
    * will evict it and not load from the cache (this is usually indicative of a
    * programming bug).
    *
    * @param module the module to deserialize from the cache.
    * @return [[Some]] when deserialization was successful, with `true` for
    *         relinking being successful and `false` otherwise. [[None]] if the
    *         cache could not be deserialized.
    */
  def deserialize(
    compiler: Compiler,
    module: Module
  ): Option[Boolean] = {
    compiler.getClass()
    if (pool.isWaitingForSerialization(module.getName)) {
      pool.abort(module.getName)

      None
    } else {
      pool.waitWhileSerializing(module.getName)

      context.loadCache(getCache(module)).toScala match {
        case Some(loadedCache) =>
          context.updateModule(
            module,
            { u =>
              u.ir(loadedCache.moduleIR)
              u.compilationStage(loadedCache.compilationStage)
              u.loadedFromCache(true)
            }
          )
          context.logSerializationManager(
            debugLogLevel,
            "Restored IR from cache for module [{0}] at stage [{1}].",
            module.getName,
            loadedCache.compilationStage
          )
          Some(true)
        case None =>
          context.logSerializationManager(
            debugLogLevel,
            "Unable to load a cache for module [{0}].",
            module.getName
          )
          None
      }
    }
  }

  /** Create the task that serializes the provided module IR when it is run.
    *
    * @param cache the cache manager for the module being serialized
    * @param ir the IR for the module being serialized
    * @param stage the compilation stage of the module
    * @param name the name of the module being serialized
    * @param source the source of the module being serialized
    * @param useGlobalCacheLocations if true, will use global caches location, local one otherwise
    * @return the task that serialies the provided `ir`
    */
  def doSerializeModule(
    cache: Cache[ModuleCache.CachedModule, ModuleCache.Metadata],
    ir: IRModule,
    stage: CompilationStage,
    name: QualifiedName,
    source: Source,
    useGlobalCacheLocations: Boolean
  ): Callable[Boolean] = { () =>
    pool.waitWhileSerializing(name)

    context.logSerializationManager(
      debugLogLevel,
      "Running serialization for module [{0}].",
      name
    )
    pool.startSerializing(name)
    try {
      val fixedStage =
        if (stage.isAtLeast(CompilationStage.AFTER_STATIC_PASSES)) {
          CompilationStage.AFTER_STATIC_PASSES
        } else stage
      context
        .saveCache(
          cache,
          new ModuleCache.CachedModule(ir, fixedStage, source),
          useGlobalCacheLocations
        )
        .map(_ => true)
        .orElse(false)
    } catch {
      case e: NotSerializableException =>
        context.logSerializationManager(
          Level.SEVERE,
          s"Could not serialize module [$name].",
          e
        )
        throw e
      case e: Throwable =>
        context.logSerializationManager(
          Level.SEVERE,
          s"Serialization of module `$name` failed: ${e.getMessage}`",
          e
        )
        throw e
    } finally {
      pool.finishSerializing(name)
    }
  }

  def shutdown(waitForPendingJobCompletion: Boolean = false): Unit =
    pool.shutdown(waitForPendingJobCompletion)

  private def getCache(
    module: Module
  ): Cache[ModuleCache.CachedModule, ModuleCache.Metadata] = {
    module.asInstanceOf[TruffleCompilerContext.Module].getCache
  }
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
