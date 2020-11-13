package org.enso.languageserver.io

import java.io.InputStream

import akka.util.ByteString
import org.enso.languageserver.io.ObservableOutputStream.OutputObserver
import org.enso.languageserver.io.ObservablePipedInputStream.{
  InputObserver,
  InputStreamEvent,
  ReadBlocked
}

/**
  * An observable piped input stream connected to a observable output stream.
  * This stream provides data bytes that are written to the output stream.
  * It notifies observers when read operation waits for output stream. The
  * stream stores buffered bytes in a rope-like immutable data structure that
  * avoids copying of arrays when concatenating and slicing sequences of bytes.
  *
  * @param sink the output stream that feed this input stream
  */
class ObservablePipedInputStream(sink: ObservableOutputStream)
    extends InputStream
    with OutputObserver {

  sink.attach(this)

  private val lock = new AnyRef

  private var observers = Set.empty[InputObserver]

  private var buffer: ByteString = ByteString.empty

  /** @inheritdoc **/
  override def read(): Int = lock.synchronized {
    waitForBuffer()
    val byte = buffer.head
    buffer = buffer.tail
    lock.notifyAll()
    byte.toInt
  }

  /** @inheritdoc **/
  override def read(array: Array[Byte]): Int = read(array, 0, array.length)

  /** @inheritdoc **/
  override def read(array: Array[Byte], off: Int, len: Int): Int =
    lock.synchronized {
      waitForBuffer()
      val sliceLength = if (buffer.length >= len) len else buffer.length
      val slice       = buffer.slice(0, sliceLength)
      buffer = buffer.drop(sliceLength)
      Array.copy(slice.toArray, 0, array, off, sliceLength)
      lock.notifyAll()
      sliceLength
    }

  private def waitForBuffer(): Unit =
    while (buffer.isEmpty) {
      notifyObservers(ReadBlocked)
      lock.wait()
    }

  /** @inheritdoc **/
  override def available(): Int = lock.synchronized {
    buffer.length
  }

  /** @inheritdoc **/
  override def update(output: Array[Byte]): Unit = lock.synchronized {
    buffer = ByteString.createBuilder
      .append(buffer)
      .append(ByteString.fromArray(output))
      .result()
    lock.notifyAll()
  }

  /**
    * Attaches an input observer.
    *
    * @param observer the observer that subscribe for input events
    */
  def attach(observer: InputObserver): Unit = lock.synchronized {
    observers += observer
  }

  /**
    * Detaches an input observer.
    *
    * @param observer the observer that was subscribed for input events
    */
  def detach(observer: InputObserver): Unit = lock.synchronized {
    observers -= observer
  }

  protected def notifyObservers(event: InputStreamEvent): Unit =
    lock.synchronized {
      observers foreach (_.update(event))
    }

}

object ObservablePipedInputStream {

  /**
    * Base trait of input stream events.
    */
  sealed trait InputStreamEvent

  /**
    * Signals that read operation is blocked and waits for data provided by
    * connected output stream.
    */
  case object ReadBlocked extends InputStreamEvent

  /**
    * Defines an updating interface for objects that should be notified of
    * events fired by the input stream.
    */
  trait InputObserver {

    /**
      * Method used to notify an observer about input events.
      *
      * @param event the input stream event
      */
    def update(event: InputStreamEvent): Unit

  }

}
