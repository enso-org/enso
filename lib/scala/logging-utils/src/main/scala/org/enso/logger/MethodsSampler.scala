package org.enso.logger

import org.netbeans.modules.sampler.Sampler

import java.io.{DataOutputStream, FileOutputStream, OutputStream}
import java.nio.file.Files
import java.util.concurrent.{CompletableFuture, Executor, Executors}

import scala.concurrent.duration.Duration
import scala.util.Using

/** Gathers application performance statistics that can be visualised in Java
  * VisualVM, and writes it to the provided output.
  *
  * @param output the output stream to write the collected statistics to
  */
final class MethodsSampler(output: OutputStream) {

  private val sampler: Sampler         = Sampler.createSampler(getClass.getSimpleName)
  private var samplingStarted: Boolean = false

  /** Start gathering the application statistics. */
  def start(): Unit =
    this.synchronized {
      if (!samplingStarted) {
        sampler.start()
        samplingStarted = true
      }
    }

  /** Stop gathering the application statistics and write it to the output. */
  def stop(): Unit =
    this.synchronized {
      if (samplingStarted) {
        samplingStarted = false
        Using.resource(new DataOutputStream(output))(sampler.stopAndWriteTo)
      }
    }

  /** Stop gathering the application statistics after the provided delay and
    * write it to the output.
    *
    * @param delay the duration to wait before stopping
    * @param ec the execution context
    */
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

  /** @return `true` if the sampling */
  def isSamplingStarted: Boolean =
    this.samplingStarted
}
object MethodsSampler {

  /** Create an instance of [[MethodsSampler]] that writes the data to the
    * temporary `.npss` file with the provided prefix.
    *
    * @param prefix the prefix of the temp file.
    * @return the [[MethodsSampler]] instance
    */
  def apply(prefix: String): MethodsSampler = {
    val path = Files.createTempFile(s"$prefix-", ".npss")
    new MethodsSampler(new FileOutputStream(path.toFile))
  }
}
