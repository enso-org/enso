package org.enso.loggingservice.internal

import java.util.concurrent.ArrayBlockingQueue

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.WSLogMessage

/**
  * A message queue that can be consumed by a thread in a loop.
  *
  * TODO [RW] doc
  */
class BlockingConsumerMessageQueue(bufferSize: Int = 5000)
    extends MessageQueue {
  override def send(message: Either[InternalLogMessage, WSLogMessage]): Unit = {
    val inserted = queue.offer(message)
    if (!inserted) {
      queue.clear()
      queue.offer(Left(queueOverflowMessage))
    }
  }

  /**
    * Returns next message in the queue, waiting if no messages are currently
    * available.
    */
  def nextMessage(): WSLogMessage = encodeMessage(queue.take())

  def drain(): Seq[WSLogMessage] = {
    val buffer = scala.collection.mutable
      .Buffer[Either[InternalLogMessage, WSLogMessage]]()
    import scala.jdk.CollectionConverters._
    queue.drainTo(buffer.asJava)
    buffer.toSeq.map(encodeMessage)
  }

  private def encodeMessage(
    message: Either[InternalLogMessage, WSLogMessage]
  ): WSLogMessage = message.fold(_.toLogMessage(), identity)

  private val queueOverflowMessage: InternalLogMessage =
    InternalLogMessage(
      level = LogLevel.Warning,
      classOf[BlockingConsumerMessageQueue].getCanonicalName,
      "The Logger does not keep up with processing log messages. " +
      "Some log messages have been dropped.",
      None
    )

  private val queue =
    new ArrayBlockingQueue[Either[InternalLogMessage, WSLogMessage]](bufferSize)
}
