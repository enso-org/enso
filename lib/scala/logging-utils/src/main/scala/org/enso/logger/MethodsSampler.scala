package org.enso.logger

import org.netbeans.modules.sampler.Sampler

import java.io.{DataOutputStream, FileOutputStream}
import java.nio.file.Files
import java.util.concurrent.{CompletableFuture, Executor, Executors}

import scala.concurrent.duration.Duration
import scala.util.Using
import java.io.File

/** Sampler gathers the application performance statistics. */
trait MethodsSampler {

  /** Start gathering the application statistics. */
  def start(): Unit

  /** Stop gathering the application statistics and write it to the output. */
  def stop(): Unit

  /** Stop gathering the application statistics after the provided delay and
    * write it to the output.
    *
    * @param delay the duration to wait before stopping
    * @param ec the execution context
    */
  def stop(delay: Duration)(implicit ec: Executor): Unit
}

/** Gathers application performance statistics that can be visualised in Java
  * VisualVM, and writes it to the provided output.
  *
  * @param output the output stream to write the collected statistics to
  */
final class OutputStreamSampler(output: File) extends MethodsSampler {

  private val sampler: Sampler         = Sampler.createSampler(getClass.getSimpleName)
  private var samplingStarted: Boolean = false

  def getSiblingFile(ext : String): File = {
    val newName = output.getName().replace(".npss", ext)
    return new File(output.getParent(), newName)
  }

  /** @inheritdoc */
  def start(): Unit =
    this.synchronized {
      if (!samplingStarted) {
        sampler.start()
        samplingStarted = true
      }
    }

  /** @inheritdoc */
  def stop(): Unit =
    this.synchronized {
      if (samplingStarted) {
        samplingStarted = false
        Using.resource(new DataOutputStream(new FileOutputStream(output)))(sampler.stopAndWriteTo)
      }
    }

  /** @inheritdoc */
  def stop(delay: Duration)(implicit ec: Executor): Unit =
    this.synchronized {
      val executor = Executors.newSingleThreadScheduledExecutor()

      CompletableFuture
        .runAsync(
          { () =>
            executor.schedule[Unit](() => this.stop(), delay.length, delay.unit)
          }: Runnable,
          ec
        )
        .whenComplete((_, _) => executor.shutdown())
    }

  /** @return `true` if the sampling is started. */
  def isSamplingStarted: Boolean =
    this.samplingStarted
}
object OutputStreamSampler {

  /** Create an instance of [[MethodsSampler]] that writes the data to the
    * temporary `.npss` file with the provided prefix.
    *
    * @param prefix the prefix of the temp file.
    * @return the [[MethodsSampler]] instance
    */
  def apply(prefix: String): OutputStreamSampler = {
    val path = Files.createTempFile(s"$prefix-", ".npss")
    new OutputStreamSampler(path.toFile)
  }
}

/** Sampler that does nothing. */
final class NoopSampler extends MethodsSampler {

  /** @inheritdoc */
  override def start(): Unit = ()

  /** @inheritdoc */
  override def stop(): Unit = ()

  /** @inheritdoc */
  override def stop(delay: Duration)(implicit ec: Executor): Unit = ()
}
object NoopSampler {

  /** Create an instance of [[NoopSampler]]. */
  def apply(): NoopSampler =
    new NoopSampler
}
