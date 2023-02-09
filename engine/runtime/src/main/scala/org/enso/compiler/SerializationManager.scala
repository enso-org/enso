package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.Module
import org.enso.pkg.QualifiedName

import java.io.NotSerializableException
import java.util.concurrent.{
  ConcurrentHashMap,
  LinkedBlockingDeque,
  ThreadPoolExecutor,
  TimeUnit
}
import java.util.logging.Level
import scala.collection.mutable

class SerializationManager(compiler: Compiler) {

  /** The debug logging level. */
  private val debugLogLevel = Level.FINE

  /** A logger for messages regarding serialization. */
  private val logger: TruffleLogger =
    compiler.context.getLogger(classOf[SerializationManager])

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
    collection.concurrent.TrieMap[QualifiedName, Runnable]()

  /** The runtime's environment. */
  private val env = compiler.context.getEnvironment

  /** The thread pool that handles serialization. */
  private val pool: ThreadPoolExecutor = new ThreadPoolExecutor(
    SerializationManager.startingThreadCount,
    SerializationManager.maximumThreadCount,
    SerializationManager.threadKeepalive,
    TimeUnit.SECONDS,
    new LinkedBlockingDeque[Runnable](),
    (runnable: Runnable) => {
      env.createSystemThread(runnable)
    }
  )

  // Make sure it is started to avoid races with language shutdown with low job
  // count.
  if (compiler.context.getEnvironment.isCreateThreadAllowed) {
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
    * @return `true` if `module` has been scheduled for serialization, `false`
    *         otherwise
    */
  def serialize(module: Module, useGlobalCacheLocations: Boolean): Boolean = {
    logger.log(
      debugLogLevel,
      s"Requesting serialization for module [${module.getName}]."
    )
    val duplicatedIr = compiler.updateMetadata(
      module.getIr,
      module.getIr.duplicate(keepIdentifiers = true)
    )
    duplicatedIr.preorder.foreach(_.passData.prepareForSerialization(compiler))

    val task = doSerialize(
      module.getCache,
      duplicatedIr,
      module.getCompilationStage,
      module.getName,
      module.getSource,
      useGlobalCacheLocations
    )
    if (compiler.context.getEnvironment.isCreateThreadAllowed) {
      isWaitingForSerialization.put(module.getName, task)
      pool.execute(task)
    } else {
      task.run()
    }

    true
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
  def deserialize(module: Module): Option[Boolean] = {
    if (isWaitingForSerialization(module)) {
      abort(module)
      None
    } else {
      while (isSerializing(module)) {
        Thread.sleep(100)
      }

      module.getCache.load(compiler.context) match {
        case Some(ModuleCache.CachedModule(ir, stage, _)) =>
          val relinkedIrChecks =
            ir.preorder.map(_.passData.restoreFromSerialization(this.compiler))
          module.unsafeSetIr(ir)
          module.unsafeSetCompilationStage(stage)
          module.setLoadedFromCache(true)
          logger.log(
            debugLogLevel,
            s"Restored IR from cache for module [${module.getName}] at stage [$stage]."
          )

          if (!relinkedIrChecks.contains(false)) {
            module.setHasCrossModuleLinks(true)
            logger.log(
              debugLogLevel,
              s"Restored links (early phase) in module [${module.getName}]."
            )
            Some(true)
          } else {
            logger.log(
              debugLogLevel,
              s"Could not restore links (early phase) in module [${module.getName}]."
            )
            module.setHasCrossModuleLinks(false)
            Some(false)
          }
        case None =>
          logger.log(
            debugLogLevel,
            s"Unable to load a cache for module [${module.getName}]."
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
  def isSerializing(module: Module): Boolean = {
    isSerializing.contains(module.getName)
  }

  /** Checks if the provided module is waiting for serialization.
    *
    * @param module the module to check
    * @return `true` if `module` is waiting for serialization, `false` otherwise
    */
  def isWaitingForSerialization(module: Module): Boolean = {
    isWaitingForSerialization.contains(module.getName)
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
  def abort(module: Module): Boolean = {
    if (isWaitingForSerialization(module)) {
      pool.remove(
        isWaitingForSerialization.remove(module.getName).getOrElse(return false)
      )
      true
    } else false
  }

  /** Performs shutdown actions for the serialization manager.
    *
    * @param waitForPendingJobCompletion whether or not shutdown should wait for
    *                                    pending serialization jobs
    */
  def shutdown(waitForPendingJobCompletion: Boolean = false): Unit = {
    if (!pool.isShutdown) {
      if (waitForPendingJobCompletion && this.hasJobsRemaining) {
        val jobCount = isWaitingForSerialization.size + isSerializing.size
        logger.log(
          debugLogLevel,
          s"Waiting for $jobCount serialization jobs to complete."
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
      logger.log(debugLogLevel, "Serialization manager has been shut down.")
    }
  }

  // === Internals ============================================================

  /** @return `true` if there are remaining serialization jobs, `false`
    *         otherwise
    */
  private def hasJobsRemaining: Boolean =
    isWaitingForSerialization.nonEmpty || isSerializing.nonEmpty

  /** Create the task that serializes the provided module IR when it is run.
    *
    * @param cache the cache manager for the module being serialized
    * @param ir the IR for the module being serialized
    * @param stage the compilation stage of the module
    * @param name the name of the module being serialized
    * @param source the source of the module being serialized
    * @return the task that serialies the provided `ir`
    */
  private def doSerialize(
    cache: ModuleCache,
    ir: IR.Module,
    stage: Module.CompilationStage,
    name: QualifiedName,
    source: Source,
    useGlobalCaches: Boolean
  ): Runnable = { () =>
    logger.log(
      debugLogLevel,
      s"Running serialization for module [$name]."
    )
    startSerializing(name)
    try {
      val fixedStage =
        if (stage.isAtLeast(Module.CompilationStage.AFTER_STATIC_PASSES)) {
          Module.CompilationStage.AFTER_STATIC_PASSES
        } else stage
      cache.save(
        ModuleCache.CachedModule(ir, fixedStage, source),
        compiler.context,
        useGlobalCaches
      )
    } catch {
      case e: NotSerializableException =>
        logger.log(
          Level.SEVERE,
          s"Could not serialize module [$name].",
          e
        )
        throw e
      case e: Throwable =>
        logger.log(
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
    isWaitingForSerialization.remove(name)
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
}
object SerializationManager {

  /** The maximum number of serialization threads allowed. */
  val maximumThreadCount: Integer = 2

  /** The number of threads at compiler start. */
  val startingThreadCount: Integer = maximumThreadCount

  /** The thread keep-alive time in seconds. */
  val threadKeepalive: Long = 3
}
