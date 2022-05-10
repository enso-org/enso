package org.enso.profiling

import org.netbeans.modules.sampler.Sampler

import java.io.{DataOutputStream, File, FileOutputStream}
import java.nio.file.Files
import java.util.concurrent.{CompletableFuture, Executor, Executors}

import scala.concurrent.duration.Duration
import scala.util.Using

/** Gathers application performance statistics that can be visualised in Java
  * VisualVM, and writes it to the provided output.
  *
  * @param output the output stream to write the collected statistics to
  */
final class TempFileSampler(output: File) extends MethodsSampler {

  private val sampler: Sampler         = Sampler.createSampler(getClass.getSimpleName)
  private var samplingStarted: Boolean = false

  def getSiblingFile(ext: String): File = {
    val newName = output.getName.replace(".npss", ext)
    new File(output.getParent, newName)
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
        Using.resource(new DataOutputStream(new FileOutputStream(output)))(
          sampler.stopAndWriteTo
        )
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
object TempFileSampler {

  /** Create an instance of [[MethodsSampler]] that writes the data to the
    * temporary `.npss` file with the provided prefix.
    *
    * @param prefix the prefix of the temp file.
    * @return the [[MethodsSampler]] instance
    */
  def apply(prefix: String): TempFileSampler = {
    val path = Files.createTempFile(s"$prefix-", ".npss")
    Files.deleteIfExists(path)
    new TempFileSampler(path.toFile)
  }
}
