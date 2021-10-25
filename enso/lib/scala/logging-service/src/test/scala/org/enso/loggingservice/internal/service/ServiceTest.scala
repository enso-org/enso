package org.enso.loggingservice.internal.service

import java.time.Instant
import java.util.concurrent.{Semaphore, TimeUnit}

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.TestLogger.TestLogMessage
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogMessage
}
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.{Printer, TestPrinter}
import org.scalatest.OptionValues
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpec

trait ServiceTest
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with TimeLimitedTests {

  override def timeLimit: Span = 20.seconds

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
    val message =
      InternalLogMessage(
        LogLevel.Debug,
        Instant.now(),
        "group",
        "message",
        None
      )
    val skippedMessage =
      InternalLogMessage(
        LogLevel.Trace,
        Instant.now(),
        "group",
        "message",
        None
      )

    queue.send(Left(message))
    val service = serviceConstructor(LogLevel.Debug, queue, Seq(testPrinter))
    queue.send(Left(skippedMessage))
    assert(
      semaphore.tryAcquire(1, 5000, TimeUnit.MILLISECONDS),
      "; Waiting for messages timed out."
    )
    service.terminate()
    assert(testPrinter.wasShutdown)

    testPrinter.getLoggedMessages shouldEqual Seq(
      TestLogMessage(LogLevel.Debug, "message")
    )
  }
}
