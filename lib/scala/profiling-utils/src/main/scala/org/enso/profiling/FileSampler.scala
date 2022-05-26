package org.enso.profiling

import org.netbeans.modules.sampler.Sampler

import java.io.{DataOutputStream, File, FileOutputStream}
import java.util.concurrent.{CompletableFuture, Executor, Executors}

import scala.concurrent.duration.FiniteDuration
import scala.util.Using

/** Gathers application performance statistics that can be visualised in Java
  * VisualVM, and writes it to the provided output.
  *
  * @param output the output stream to write the collected statistics to
  */
final class FileSampler(output: File) extends MethodsSampler {

  private val sampler: Sampler         = Sampler.createSampler(getClass.getSimpleName)
  private var samplingStarted: Boolean = false

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
        Using.resource(new DataOutputStream(new FileOutputStream(output)))(
          sampler.stopAndWriteTo
        )
      }
    }

  /** @inheritdoc */
  def stop(delay: FiniteDuration)(implicit ec: Executor): Unit =
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
}
