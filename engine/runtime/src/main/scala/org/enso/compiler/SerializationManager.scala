package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
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
      env.createThread(runnable)
    }
  )

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
    * @param module the module to serialize
    * @return `true` if `module` has been scheduled for serialization, `false`
    *         otherwise
    */
  def serialize(module: Module): Boolean = {
    logger.log(
      debugLogLevel,
      s"Requesting serialization for module [${module.getName}]."
    )
    val duplicatedIr = module.getIr.duplicate()
    duplicatedIr.preorder.foreach(_.passData.prepareForSerialization(compiler))

    val task = doSerialize(
      module.getCache,
      duplicatedIr,
      module.getCompilationStage,
      module.getName
    )
    if (compiler.context.getEnvironment.isCreateThreadAllowed) {
      isWaitingForSerialization.put(module.getName, task)
      pool.execute(task)
    } else {
      task.run()
    }

    true
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

        while (this.hasJobsRemaining) {
          Thread.sleep(1 * 1000)
        }
      }

      pool.shutdown()

      while (!pool.isTerminated) {
        pool.awaitTermination(500, TimeUnit.MILLISECONDS)
      }
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
    * @return the task that serialies the provided `ir`
    */
  private def doSerialize(
    cache: ModuleCache,
    ir: IR.Module,
    stage: Module.CompilationStage,
    name: QualifiedName
  ): Runnable = { () =>
    startSerializing(name)
    logger.log(
      debugLogLevel,
      s"Running serialization for module [$name]."
    )
    try {
      val fixedStage =
        if (stage.isAtLeast(Module.CompilationStage.AFTER_STATIC_PASSES)) {
          Module.CompilationStage.AFTER_STATIC_PASSES
        } else stage
      cache.save(ModuleCache.CachedModule(ir, fixedStage), compiler.context)
    } catch {
      case e: NotSerializableException =>
        logger.log(
          Level.SEVERE,
          s"Could not serialize module [$name]."
        )
        throw e
    }
    finishSerializing(name)
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
