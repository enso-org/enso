package org.enso.languageserver.profiling

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.distribution.DistributionManager
import org.enso.profiling.sampler.{MethodsSampler, OutputStreamSampler}

import java.io.ByteArrayOutputStream
import java.nio.file.Files
import java.time.{Clock, Instant}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import scala.util.{Failure, Success, Try}

/** Handles the profiling commands.
  *
  * @param distributionManager the distribution manager
  * @param clock the system clock
  */
final class ProfilingManager(
  distributionManager: DistributionManager,
  clock: Clock
) extends Actor
    with LazyLogging {

  import ProfilingManager._

  override def receive: Receive =
    initialized(None)

  private def initialized(sampler: Option[RunningSampler]): Receive = {
    case ProfilingProtocol.ProfilingStartRequest =>
      sampler match {
        case Some(_) =>
          sender() ! ProfilingProtocol.ProfilingStartResponse
        case None =>
          val result  = new ByteArrayOutputStream()
          val sampler = new OutputStreamSampler(result)

          sampler.start()

          sender() ! ProfilingProtocol.ProfilingStartResponse
          context.become(initialized(Some(RunningSampler(sampler, result))))
      }

    case ProfilingProtocol.ProfilingStopRequest =>
      sampler match {
        case Some(RunningSampler(sampler, result)) =>
          sampler.stop()

          Try(saveProfilingResult(result.toByteArray, clock.instant())) match {
            case Failure(exception) =>
              logger.error("Failed to save profiling result.", exception)
            case Success(()) =>
          }

          sender() ! ProfilingProtocol.ProfilingStopResponse
          context.become(initialized(None))
        case None =>
          sender() ! ProfilingProtocol.ProfilingStopResponse
      }
  }

  private def saveProfilingResult(
    result: Array[Byte],
    instant: Instant
  ): Unit = {
    val fileName = createProfilingFileName(instant)
    val path     = distributionManager.paths.profiling.resolve(fileName)
    Files.write(path, result)
  }
}

object ProfilingManager {

  private val SAMPLES_FILE_PREFIX = "samples"
  private val SAMPLES_FILE_EXT    = ".npss"

  private val PROFILING_FILE_DATE_PART_FORMATTER =
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral('-')
      .appendValue(ChronoField.HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
      .optionalStart()
      .appendLiteral(':')
      .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
      .toFormatter()

  private case class RunningSampler(
    sampler: MethodsSampler,
    result: ByteArrayOutputStream
  )

  private def createProfilingFileName(instant: Instant): String = {
    val datePart = PROFILING_FILE_DATE_PART_FORMATTER.format(instant)
    s"$SAMPLES_FILE_PREFIX-$datePart$SAMPLES_FILE_EXT"
  }

  /** Creates the configuration object used to create a [[ProfilingManager]].
    *
    * @param distributionManager the distribution manager
    * @param clock the system clock
    */
  def props(
    distributionManager: DistributionManager,
    clock: Clock = Clock.systemDefaultZone()
  ): Props =
    Props(new ProfilingManager(distributionManager, clock))
}
