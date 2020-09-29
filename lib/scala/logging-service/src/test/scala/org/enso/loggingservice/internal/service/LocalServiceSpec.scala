package org.enso.loggingservice.internal.service

import java.time.Instant
import java.util.concurrent.{Semaphore, TimeUnit}

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.TestLogger.TestLogMessage
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.{Printer, TestPrinter}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LocalServiceSpec extends AnyWordSpec with Matchers with OptionValues {
  "Local service" should {
    "gather messages" in {
      LocalServiceSpec.testServiceMessageGathering {
        (logLevel, queue, printers) =>
          Local.setup(logLevel, queue, printers)
      }
    }
  }
}

object LocalServiceSpec extends Matchers {
  def testServiceMessageGathering(
    serviceConstructor: (
      LogLevel,
      BlockingConsumerMessageQueue,
      Seq[Printer]
    ) => Service
  ): Unit = {
    val queue     = new BlockingConsumerMessageQueue()
    val semaphore = new Semaphore(0)
    val testPrinter = new TestPrinter {
      override def print(message: WSLogMessage): Unit = {
        super.print(message)
        semaphore.release()
      }
    }
    val service = serviceConstructor(LogLevel.Debug, queue, Seq(testPrinter))
    val message =
      WSLogMessage(LogLevel.Debug, Instant.now(), "group", "message", None)
    val skippedMessage =
      WSLogMessage(LogLevel.Trace, Instant.now(), "group", "message", None)
    queue.send(Right(skippedMessage))
    queue.send(Right(message))
    assert(
      semaphore.tryAcquire(100, TimeUnit.MILLISECONDS),
      "message is received"
    )
    service.terminate()
    assert(testPrinter.wasShutdown())

    testPrinter.getLoggedMessages() shouldEqual Seq(
      TestLogMessage(LogLevel.Debug, "message")
    )
  }
}
