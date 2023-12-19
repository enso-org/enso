package org.enso.languageserver.websocket.json

import org.enso.distribution.DistributionManager
import org.enso.languageserver.profiling.ProfilingManager
import org.enso.languageserver.runtime.RuntimeConnector

import java.nio.file.Files

class ProfilingManagerTest extends BaseServerTest {

  private val json = ProfilingJsonMessages

  def getDistributionManager =
    new DistributionManager(fakeInstalledEnvironment())

  "ProfilingManager" should {

    "save profiling data " in {
      val client = getInitialisedWsClient()

      client.send(json.profilingStart(1))
      runtimeConnectorProbe.receiveN(1).head match {
        case _: RuntimeConnector.RegisterEventsMonitor =>
        // Ok
        case other =>
          fail(s"Unexpected message: $other")
      }
      client.expectJson(json.ok(1))

      client.send(json.profilingStop(2))
      runtimeConnectorProbe.receiveN(1).head match {
        case _: RuntimeConnector.RegisterEventsMonitor =>
        // Ok
        case other =>
          fail(s"Unexpected message: $other")
      }
      client.expectJson(json.ok(2))

      val distributionManager = getDistributionManager
      val instant             = clock.instant
      val samplesFile = distributionManager.paths.profiling.resolve(
        ProfilingManager.createSamplesFileName(instant)
      )
      val eventsFile = distributionManager.paths.profiling.resolve(
        ProfilingManager.createEventsFileName(instant)
      )

      Files.exists(samplesFile) shouldEqual true
      Files.exists(eventsFile) shouldEqual true
    }

    "save profiling with memory snapshot " in {
      val client = getInitialisedWsClient()

      client.send(json.profilingStart(1, memorySnapshot = true))
      runtimeConnectorProbe.receiveN(1).head match {
        case _: RuntimeConnector.RegisterEventsMonitor =>
        // Ok
        case other =>
          fail(s"Unexpected message: $other")
      }
      client.expectJson(json.ok(1))

      client.send(json.profilingStop(2))
      runtimeConnectorProbe.receiveN(1).head match {
        case _: RuntimeConnector.RegisterEventsMonitor =>
        // Ok
        case other =>
          fail(s"Unexpected message: $other")
      }
      client.expectJson(json.ok(2))

      val distributionManager = getDistributionManager
      val instant             = clock.instant
      val samplesFile = distributionManager.paths.profiling.resolve(
        ProfilingManager.createSamplesFileName(instant)
      )
      val eventsFile = distributionManager.paths.profiling.resolve(
        ProfilingManager.createEventsFileName(instant)
      )
      val snapshotFile = distributionManager.paths.profiling.resolve(
        ProfilingManager.createHeapDumpFileName(instant)
      )

      Files.exists(samplesFile) shouldEqual true
      Files.exists(eventsFile) shouldEqual true
      Files.exists(snapshotFile) shouldEqual true
    }

    "save memory snapshot" in {
      val client = getInitialisedWsClient()

      client.send(json.profilingSnapshot(1))
      client.expectJson(json.ok(1))

      val distributionManager = getDistributionManager
      val instant             = clock.instant
      val snapshotFile = distributionManager.paths.profiling.resolve(
        ProfilingManager.createHeapDumpFileName(instant)
      )

      Files.exists(snapshotFile) shouldEqual true
    }
  }

}
