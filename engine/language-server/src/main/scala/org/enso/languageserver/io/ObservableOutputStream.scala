package org.enso.languageserver.io

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary

import java.io.OutputStream
import org.enso.languageserver.io.ObservableOutputStream.OutputObserver

/** An observable output stream of bytes. It accepts output bytes
  * and sends them to attached observers. Observers can dynamically
  * subscribe and unsubscribe for output bytes. It's thread-safe.
  */
class ObservableOutputStream extends OutputStream {

  private val lock = new AnyRef

  private var observers = Set.empty[OutputObserver]

  /** @inheritdoc */
  @TruffleBoundary
  override def write(byte: Int): Unit = lock.synchronized {
    notify(Array[Byte](byte.toByte))
  }

  /** @inheritdoc */
  @TruffleBoundary
  override def write(bytes: Array[Byte]): Unit = lock.synchronized {
    if (bytes.length > 0) {
      notify(bytes)
    }
  }

  /** @inheritdoc */
  @TruffleBoundary
  override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
    lock.synchronized {
      if (len > 0) {
        val buf = new Array[Byte](len)
        Array.copy(bytes, off, buf, 0, len)
        notify(buf)
      }
    }

  /** Attaches an output observer.
    *
    * @param observer the observer that subscribe for output bytes
    */
  def attach(observer: OutputObserver): Unit = lock.synchronized {
    observers += observer
  }

  /** Detaches an output observer.
    *
    * @param observer the observer that was subscribed for output bytes
    */
  def detach(observer: OutputObserver): Unit = lock.synchronized {
    observers -= observer
  }

  @TruffleBoundary
  protected def notify(output: Array[Byte]): Unit = {
    observers foreach { _.update(output) }
  }

}

object ObservableOutputStream {

  /** Defines an updating interface for objects that should be notified of new
    * data accepted by an output stream.
    */
  trait OutputObserver {

    /** Method used to notify an observer about output changes.
      *
      * @param output the new data
      */
    def update(output: Array[Byte]): Unit

  }

}
