package org.enso.interpreter.runtime

import com.oracle.truffle.api.source.Source
import org.enso.compiler.Compiler
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{Module => IRModule}
import org.enso.compiler.context.{ExportsBuilder, ExportsMap, SuggestionBuilder}
import org.enso.compiler.context.CompilerContext
import org.enso.compiler.context.CompilerContext.Module
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.editions.LibraryName
import org.enso.pkg.QualifiedName
import org.enso.polyglot.Suggestion
import org.enso.polyglot.CompilationStage
import org.enso.interpreter.caches.ImportExportCache
import org.enso.interpreter.caches.ModuleCache
import org.enso.interpreter.caches.SuggestionsCache

import java.io.NotSerializableException
import java.util
import java.util.concurrent.{
  Callable,
  CompletableFuture,
  ConcurrentHashMap,
  Future,
  LinkedBlockingDeque,
  ThreadPoolExecutor,
  TimeUnit
}
import java.util.logging.Level

import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional

final class SerializationManager(private val context: TruffleCompilerContext) {

  def this(compiler: Compiler) = {
    this(compiler.context.asInstanceOf[TruffleCompilerContext])
  }

  import SerializationManager._

  /** The debug logging level. */
  private val debugLogLevel = Level.FINE

  /** A set of the modules that are currently being serialized.
    *
    * This set is accessed concurrently. This is safe as it is backed by a
    * [[ConcurrentHashMap]] and is wrapped with the scala [[mutable.Set]]
    * interface.
    */
  private val isSerializing: mutable.Set[QualifiedName] = buildConcurrentHashSet

  /** A map of the modules awaiting serialization to their associated tasks
    *
    * This map is accessed concurrently.
    */
  private val isWaitingForSerialization =
    collection.concurrent.TrieMap[QualifiedName, Future[Boolean]]()

  /** The thread pool that handles serialization. */
  private val pool: ThreadPoolExecutor = new ThreadPoolExecutor(
    SerializationManager.startingThreadCount,
    SerializationManager.maximumThreadCount,
    SerializationManager.threadKeepalive,
    TimeUnit.SECONDS,
    new LinkedBlockingDeque[Runnable](),
    (runnable: Runnable) => {
      context.createSystemThread(runnable)
    }
  )

  // Make sure it is started to avoid races with language shutdown with low job
  // count.
  if (context.isCreateThreadAllowed) {
    pool.prestartAllCoreThreads()
  }

  // === Interface ============================================================

  /** Requests that `module` be serialized.
    *
    * This method will attempt to schedule the provided module and IR for
    * serialization regardless of whether or not it is appropriate to do so. If
    * there are preconditions needed for serialization, these should be checked
    * before calling this method.
    *
    * In addition, this method handles breaking links between modules contained
    * in the IR to ensure safe serialization.
    *
    * It is responsible for taking a "snapshot" of the relevant module state at
    * the point at which serialization is requested. This is due to the fact
    * that serialization happens in a separate thread and the module may be
    * mutated beneath it.
    *
    * @param module the module to serialize
    * @param useGlobalCacheLocations if true, will use global caches location, local one otherwise
    * @param useThreadPool if true, will perform serialization asynchronously
    * @return Future referencing the serialization task. On completion Future will return
    *         `true` if `module` has been successfully serialized, `false` otherwise
    */
  def serializeModule(
    compiler: Compiler,
    module: Module,
    useGlobalCacheLocations: Boolean,
    useThreadPool: Boolean = true
  ): Future[Boolean] = {
    if (module.isSynthetic) {
      throw new IllegalStateException(
        "Cannot serialize synthetic module [" + module.getName + "]"
      );
    }
    context.logSerializationManager(
      debugLogLevel,
      "Requesting serialization for module [{0}].",
      module.getName
    )
    val duplicatedIr = compiler.updateMetadata(
      module.getIr,
      module.getIr.duplicate(keepIdentifiers = true)
    )
    val task = doSerializeModule(
      getCache(module),
      duplicatedIr,
      module.getCompilationStage,
      module.getName,
      module.getSource,
      useGlobalCacheLocations
    )
    if (useThreadPool) {
      isWaitingForSerialization.synchronized {
        val future = pool.submit(task)
        isWaitingForSerialization.put(module.getName, future)
        future
      }
    } else {
      try {
        CompletableFuture.completedFuture(task.call())
      } catch {
        case e: Throwable =>
          context.logSerializationManager(
            debugLogLevel,
            s"Serialization task failed in module [${module.getName}].",
            e
          )
          CompletableFuture.completedFuture(false)
      }
    }
  }

  def serializeLibrary(
    compiler: Compiler,
    libraryName: LibraryName,
    useGlobalCacheLocations: Boolean
  ): Future[Boolean] = {
    context.logSerializationManager(
      Level.INFO,
      "Requesting serialization for library [{0}].",
      libraryName
    )

    val task: Callable[Boolean] =
      doSerializeLibrary(compiler, libraryName, useGlobalCacheLocations)
    if (context.isCreateThreadAllowed) {
      isWaitingForSerialization.synchronized {
        val future = pool.submit(task)
        isWaitingForSerialization.put(libraryName.toQualifiedName, future)
        future
      }
    } else {
      try {
        CompletableFuture.completedFuture(task.call())
      } catch {
        case e: Throwable =>
          context.logSerializationManager(
            debugLogLevel,
            s"Serialization task failed for library [$libraryName].",
            e
          )
          CompletableFuture.completedFuture(false)
      }
    }
  }

  private def doSerializeLibrary(
    compiler: Compiler,
    libraryName: LibraryName,
    useGlobalCacheLocations: Boolean
  ): Callable[Boolean] = () => {
    while (isSerializingLibrary(libraryName)) {
      Thread.sleep(100)
    }

    context.logSerializationManager(
      debugLogLevel,
      "Running serialization for bindings [{0}].",
      libraryName
    )
    startSerializing(libraryName.toQualifiedName)
    val bindingsCache = new ImportExportCache.CachedBindings(
      libraryName,
      new ImportExportCache.MapToBindings(
        context
          .getPackageRepository()
          .getModulesForLibrary(libraryName)
          .map { module =>
            val ir = module.getIr
            val bindings = ir.unsafeGetMetadata(
              BindingAnalysis,
              "Non-parsed module used in ImportResolver"
            )
            val abstractBindings =
              bindings.prepareForSerialization(compiler.context)
            (
              module.getName,
              org.enso.persist.Persistance.Reference.of(abstractBindings)
            )
          }
          .toMap
      ),
      context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(_.listSourcesJava())
    )
    try {
      val result =
        try {
          val cache = new ImportExportCache(libraryName)
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
      finishSerializing(libraryName.toQualifiedName)
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
      val cache = new SuggestionsCache(libraryName)
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
    if (isWaitingForSerialization(libraryName)) {
      abort(libraryName)
      None
    } else {
      while (isSerializingLibrary(libraryName)) {
        Thread.sleep(100)
      }
      val cache = new SuggestionsCache(libraryName)
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
    if (isWaitingForSerialization(libraryName)) {
      abort(libraryName)
      None
    } else {
      while (isSerializingLibrary(libraryName)) {
        Thread.sleep(100)
      }
      val cache = new ImportExportCache(libraryName)
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
    if (isWaitingForSerialization(module)) {
      abort(module)
      None
    } else {
      while (isSerializingModule(module.getName)) {
        Thread.sleep(100)
      }

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

  /** Checks if the provided module is in the process of being serialized.
    *
    * @param module the module to check
    * @return `true` if `module` is currently being serialized, `false`
    *         otherwise
    */
  private def isSerializingModule(module: QualifiedName): Boolean = {
    isSerializing.contains(module)
  }

  private def isSerializingLibrary(library: LibraryName): Boolean = {
    isSerializing.contains(library.toQualifiedName)
  }

  private def isWaitingForSerialization(name: QualifiedName): Boolean = {
    isWaitingForSerialization.synchronized {
      isWaitingForSerialization.contains(name)
    }
  }

  /** Checks if the provided module is waiting for serialization.
    *
    * @param module the module to check
    * @return `true` if `module` is waiting for serialization, `false` otherwise
    */
  private def isWaitingForSerialization(
    module: Module
  ): Boolean = {
    isWaitingForSerialization(module.getName)
  }

  /** Checks if the provided library's bindings are waiting for serialization.
    *
    * @param library the library to check
    * @return `true` if `library` is waiting for serialization, `false` otherwise
    */
  private def isWaitingForSerialization(library: LibraryName): Boolean = {
    isWaitingForSerialization(library.toQualifiedName)
  }

  private def abort(name: QualifiedName): Boolean = {
    isWaitingForSerialization.synchronized {
      if (isWaitingForSerialization(name)) {
        isWaitingForSerialization
          .remove(name)
          .map(_.cancel(false))
          .getOrElse(false)
      } else false
    }
  }

  /** Requests that serialization of `module` be aborted.
    *
    * If the module is already in the process of serialization it will not be
    * aborted.
    *
    * @param module the module for which to abort serialization
    * @return `true` if serialization for `module` was aborted, `false`
    *         otherwise
    */
  private def abort(module: Module): Boolean = {
    abort(module.getName)
  }

  /** Requests that serialization of library's bindings be aborted.
    *
    * If the library is already in the process of serialization it will not be
    * aborted.
    *
    * @param library the library for which to abort serialization
    * @return `true` if serialization for `library` was aborted, `false`
    *         otherwise
    */
  private def abort(library: LibraryName): Boolean = {
    abort(library.toQualifiedName)
  }

  /** Performs shutdown actions for the serialization manager.
    *
    * @param waitForPendingJobCompletion whether or not shutdown should wait for
    *                                    pending serialization jobs
    */
  def shutdown(waitForPendingJobCompletion: Boolean = false): Unit = {
    if (!pool.isShutdown) {
      if (waitForPendingJobCompletion && this.hasJobsRemaining) {
        val waitingCount = isWaitingForSerialization.synchronized {
          isWaitingForSerialization.size
        }
        val jobCount = waitingCount + isSerializing.size
        context.logSerializationManager(
          debugLogLevel,
          "Waiting for #{0} serialization jobs to complete.",
          jobCount
        )

        // Bound the waiting loop
        val maxCount = 60
        var counter  = 0
        while (this.hasJobsRemaining && counter < maxCount) {
          counter += 1
          Thread.sleep(1 * 1000)
        }
      }

      pool.shutdown()

      // Bound the waiting loop
      val maxCount = 10
      var counter  = 0
      while (!pool.isTerminated && counter < maxCount) {
        pool.awaitTermination(500, TimeUnit.MILLISECONDS)
        counter += 1
      }

      pool.shutdownNow()
      Thread.sleep(100)
      context.logSerializationManager(
        debugLogLevel,
        "Serialization manager has been shut down."
      )
    }
  }

  // === Internals ============================================================

  /** @return `true` if there are remaining serialization jobs, `false`
    *         otherwise
    */
  private def hasJobsRemaining: Boolean = {
    isWaitingForSerialization.synchronized {
      isWaitingForSerialization.nonEmpty || isSerializing.nonEmpty
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
  private def doSerializeModule(
    cache: ModuleCache,
    ir: IRModule,
    stage: CompilationStage,
    name: QualifiedName,
    source: Source,
    useGlobalCacheLocations: Boolean
  ): Callable[Boolean] = { () =>
    while (isSerializingModule(name)) {
      Thread.sleep(100)
    }

    context.logSerializationManager(
      debugLogLevel,
      "Running serialization for module [{0}].",
      name
    )
    startSerializing(name)
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
      finishSerializing(name)
    }
  }

  /** Sets the module described by `name` as serializing.
    *
    * @param name the name of the module to set as serializing
    */
  private def startSerializing(name: QualifiedName): Unit = {
    isWaitingForSerialization.synchronized {
      isWaitingForSerialization.remove(name)
    }
    isSerializing.add(name)
  }

  /** Sets the module described by `name` as finished with serialization.
    *
    * @param name the name of the module to set as having finished serialization
    */
  private def finishSerializing(name: QualifiedName): Unit = {
    isSerializing.remove(name)
  }

  /** Builds a [[mutable.Set]] that is backed by a [[ConcurrentHashMap]] and is
    * hence safe for concurrent access.
    *
    * @tparam T the type of the set elements
    * @return a concurrent [[mutable.Set]]
    */
  private def buildConcurrentHashSet[T]: mutable.Set[T] = {
    import scala.jdk.CollectionConverters._
    java.util.Collections
      .newSetFromMap(
        new ConcurrentHashMap[T, java.lang.Boolean]()
      )
      .asScala
  }

  private def getCache(module: Module): ModuleCache = {
    module.asInstanceOf[TruffleCompilerContext.Module].getCache
  }
}

object SerializationManager {

  /** The maximum number of serialization threads allowed. */
  val maximumThreadCount: Integer = 2

  /** The number of threads at compiler start. */
  val startingThreadCount: Integer = maximumThreadCount

  /** The thread keep-alive time in seconds. */
  val threadKeepalive: Long = 3

  implicit private class LibraryOps(val libraryName: LibraryName)
      extends AnyVal {
    def toQualifiedName: QualifiedName =
      QualifiedName(List(libraryName.namespace), libraryName.name)
  }

  def apply(context: CompilerContext): SerializationManager = {
    context.asInstanceOf[TruffleCompilerContext].getSerializationManager()
  }
}
