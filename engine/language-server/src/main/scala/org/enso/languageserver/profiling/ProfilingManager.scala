package org.enso.languageserver.profiling

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.distribution.DistributionManager
import org.enso.languageserver.runtime.RuntimeConnector
import org.enso.languageserver.runtime.events.RuntimeEventsMonitor
import org.enso.logger.masking.MaskedPath
import org.enso.profiling.events.NoopEventsMonitor
import org.enso.profiling.sampler.{MethodsSampler, OutputStreamSampler}
import org.enso.profiling.snapshot.{HeapDumpSnapshot, ProfilingSnapshot}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.{Clock, Instant, ZoneOffset}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import scala.util.{Failure, Success, Try}

/** Handles the profiling commands.
  *
  * @param runtimeConnector the connection to runtime
  * @param distributionManager the distribution manager
  * @param profilingSnapshot the profiling snapshot generator
  * @param clock the system clock
  */
final class ProfilingManager(
  runtimeConnector: ActorRef,
  distributionManager: DistributionManager,
  profilingSnapshot: ProfilingSnapshot,
  clock: Clock
) extends Actor
    with LazyLogging {

  import ProfilingManager._

  override def preStart(): Unit = {
    Files.createDirectories(distributionManager.paths.profiling)
  }

  override def receive: Receive =
    initialized(None)

  private def initialized(sampler: Option[RunningSampler]): Receive = {
    case ProfilingProtocol.ProfilingStartRequest(memorySnapshot) =>
      sampler match {
        case Some(_) =>
          sender() ! ProfilingProtocol.ProfilingStartResponse
        case None =>
          val instant = clock.instant()
          val result  = new ByteArrayOutputStream()
          val sampler = new OutputStreamSampler(result)

          sampler.start()

          val eventsMonitor = createEventsMonitor(instant)
          runtimeConnector ! RuntimeConnector.RegisterEventsMonitor(
            eventsMonitor
          )

          sender() ! ProfilingProtocol.ProfilingStartResponse
          context.become(
            initialized(
              Some(RunningSampler(instant, sampler, result, memorySnapshot))
            )
          )
      }

    case ProfilingProtocol.ProfilingStopRequest =>
      sampler match {
        case Some(RunningSampler(instant, sampler, result, memorySnapshot)) =>
          sampler.stop()

          Try(saveSamplerResult(result.toByteArray, instant)) match {
            case Failure(exception) =>
              logger.error("Failed to save the sampler's result.", exception)
            case Success(samplesPath) =>
              logger.trace(
                "Saved the sampler's result to [{}].",
                MaskedPath(samplesPath)
              )
          }

          if (memorySnapshot) {
            saveMemorySnapshot(instant)
          }

          runtimeConnector ! RuntimeConnector.RegisterEventsMonitor(
            new NoopEventsMonitor
          )

          sender() ! ProfilingProtocol.ProfilingStopResponse
          context.become(initialized(None))
        case None =>
          sender() ! ProfilingProtocol.ProfilingStopResponse
      }

    case ProfilingProtocol.ProfilingSnapshotRequest =>
      val instant = clock.instant()
      saveMemorySnapshot(instant)

      sender() ! ProfilingProtocol.ProfilingSnapshotResponse
  }

  private def saveMemorySnapshot(instant: Instant): Unit =
    Try(saveHeapDump(instant)) match {
      case Failure(exception) =>
        logger.error("Failed to save the memory snapshot.", exception)
      case Success(heapDumpPath) =>
        logger.trace(
          "Saved the memory snapshot to [{}].",
          MaskedPath(heapDumpPath)
        )
    }

  private def saveSamplerResult(
    result: Array[Byte],
    instant: Instant
  ): Path = {
    val samplesFileName = createSamplesFileName(instant)
    val samplesPath =
      distributionManager.paths.profiling.resolve(samplesFileName)

    Files.write(samplesPath, result)

    samplesPath
  }

  private def saveHeapDump(instant: Instant): Path = {
    val heapDumpFileName = createHeapDumpFileName(instant)
    val heapDumpPath =
      distributionManager.paths.profiling.resolve(heapDumpFileName)

    profilingSnapshot.generateSnapshot(heapDumpPath)

    heapDumpPath
  }

  private def createEventsMonitor(instant: Instant): RuntimeEventsMonitor = {
    val eventsLogFileName = createEventsFileName(instant)
    val eventsLogPath =
      distributionManager.paths.profiling.resolve(eventsLogFileName)
    val out = new PrintStream(eventsLogPath.toFile, StandardCharsets.UTF_8)
    new RuntimeEventsMonitor(out)
  }
}

object ProfilingManager {

  private val PROFILING_FILE_PREFIX = "enso-language-server"
  private val SAMPLES_FILE_EXT      = ".npss"
  private val EVENTS_FILE_EXT       = ".log"
  private val HEAP_DUMP_FILE_EXT    = ".hprof"

  private val PROFILING_FILE_DATE_PART_FORMATTER =
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral('T')
      .appendValue(ChronoField.HOUR_OF_DAY, 2)
      .appendLiteral('-')
      .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
      .optionalStart()
      .appendLiteral('-')
      .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
      .toFormatter()
      .withZone(ZoneOffset.UTC)

  private case class RunningSampler(
    instant: Instant,
    sampler: MethodsSampler,
    result: ByteArrayOutputStream,
    memorySnapshot: Boolean
  )

  private def createProfilingFileName(instant: Instant): String = {
    val datePart = PROFILING_FILE_DATE_PART_FORMATTER.format(instant)
    s"$PROFILING_FILE_PREFIX-$datePart"
  }

  def createSamplesFileName(instant: Instant): String = {
    val baseName = createProfilingFileName(instant)
    s"$baseName$SAMPLES_FILE_EXT"
  }

  def createEventsFileName(instant: Instant): String = {
    val baseName = createProfilingFileName(instant)
    s"$baseName$EVENTS_FILE_EXT"
  }

  def createHeapDumpFileName(instant: Instant): String = {
    val baseName = createProfilingFileName(instant)
    s"$baseName$HEAP_DUMP_FILE_EXT"
  }

  /** Creates the configuration object used to create a [[ProfilingManager]].
    *
    * @param runtimeConnector the connection to runtime
    * @param distributionManager the distribution manager
    * @param profilingSnapshot the profiling snapshot generator
    * @param clock the system clock
    */
  def props(
    runtimeConnector: ActorRef,
    distributionManager: DistributionManager,
    profilingSnapshot: ProfilingSnapshot = new HeapDumpSnapshot(),
    clock: Clock                         = Clock.systemUTC()
  ): Props =
    Props(
      new ProfilingManager(
        runtimeConnector,
        distributionManager,
        profilingSnapshot,
        clock
      )
    )
}
