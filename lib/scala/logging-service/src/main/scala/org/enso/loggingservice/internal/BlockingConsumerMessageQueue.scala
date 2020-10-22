package org.enso.loggingservice.internal

import java.util.concurrent.ArrayBlockingQueue

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.WSLogMessage

import scala.annotation.tailrec

/** A message queue that can be consumed by a thread in a loop with a limited
  * buffer.
  */
class BlockingConsumerMessageQueue(bufferSize: Int = 5000) {

  /** Enqueues the `message` to be sent and returns immediately.
    *
    * If any underlying buffers are full, they may be removed and a warning will
    * be issued.
    */
  def send(message: Either[InternalLogMessage, WSLogMessage]): Unit = {
    val inserted = queue.offer(message)
    if (!inserted) {
      queue.clear()
      queue.offer(Left(queueOverflowMessage))
    }
  }

  /** Returns next message in the queue, skipping messages that should be
    * ignored and waiting if no messages are currently available.
    *
    * The distinction between internal and external messages is that internal
    * messages should only be considered if they have log level that is enabled.
    * However, all external log messages should be processed, regardless of
    * their log level, because external messages come from other components
    * whose log level is set independently.
    */
  @tailrec
  final def nextMessage(internalLogLevel: LogLevel): WSLogMessage = {
    val (message, internal) = encodeMessage(queue.take())
    if (isMessageRelevant(internalLogLevel)(message, internal))
      message
    else nextMessage(internalLogLevel)
  }

  /** Returns all currently enqueued messages, skipping ones that should be
    * ignored.
    *
    * See [[nextMessage]] for explanation which messages are ignored.
    */
  def drain(internalLogLevel: LogLevel): Seq[WSLogMessage] = {
    val buffer = scala.collection.mutable
      .Buffer[Either[InternalLogMessage, WSLogMessage]]()
    import scala.jdk.CollectionConverters._
    queue.drainTo(buffer.asJava)
    buffer.toSeq
      .map(encodeMessage)
      .filter((isMessageRelevant(internalLogLevel) _).tupled)
      .map(_._1)
  }

  /** All external messages are relevant, but internal messages relevancy depends
    * on its log level.
    */
  private def isMessageRelevant(
    internalLogLevel: LogLevel
  )(message: WSLogMessage, internal: Boolean): Boolean =
    !internal || internalLogLevel.shouldLog(message.level)

  /** Returns the encoded message and a boolean value indicating if it was
    * internal.
    */
  private def encodeMessage(
    message: Either[InternalLogMessage, WSLogMessage]
  ): (WSLogMessage, Boolean) =
    message.fold(msg => (msg.toLogMessage, true), (_, false))

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
