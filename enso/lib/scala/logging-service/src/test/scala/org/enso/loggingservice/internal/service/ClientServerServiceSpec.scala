package org.enso.loggingservice.internal.service

import java.time.Instant
import java.util.concurrent.{Semaphore, TimeUnit}

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.TestLogger.TestLogMessage
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogMessage
}
import org.enso.loggingservice.printers.TestPrinter

class ClientServerServiceSpec extends ServiceTest {
  "Client and Server" should {
    "communicate" in {
      val serverQueue = new BlockingConsumerMessageQueue()
      val clientQueue = new BlockingConsumerMessageQueue()
      val semaphore   = new Semaphore(0)
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

      val server =
        Server.setup(
          "localhost",
          0,
          serverQueue,
          Seq(testPrinter),
          LogLevel.Off
        )
      try {
        val uri    = server.getBinding().toUri()
        val client = Client.setup(uri, clientQueue, LogLevel.Debug)
        try {
          clientQueue.send(Left(message))
          clientQueue.send(Left(skippedMessage))
          assert(
            semaphore.tryAcquire(1, 5000, TimeUnit.MILLISECONDS),
            "; Waiting for messages timed out."
          )
          testPrinter.getLoggedMessages shouldEqual Seq(
            TestLogMessage(LogLevel.Debug, "message")
          )
        } finally {
          client.terminate()
        }
      } finally {
        server.terminate()
      }
    }
  }

  "Server" should {
    "also gather local messages" in {
      testServiceMessageGathering { (logLevel, queue, printers) =>
        Server.setup("localhost", 0, queue, printers, logLevel)
      }
    }
  }
}
