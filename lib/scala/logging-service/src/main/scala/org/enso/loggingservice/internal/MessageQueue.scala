package org.enso.loggingservice.internal

trait MessageQueue {

  /**
    * Enqueues the `message` to be sent and returns immediately.
    *
    * If any underlying buffers are full, they may be removed and a warning will
    * be issued.
    */
  def send(message: Either[InternalLogMessage, WSLogMessage]): Unit
}
