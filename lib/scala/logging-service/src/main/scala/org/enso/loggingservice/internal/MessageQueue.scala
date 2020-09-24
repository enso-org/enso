package org.enso.loggingservice.internal

import org.enso.loggingservice.internal.protocol.WSLogMessage

trait MessageQueue {

  /**
    * Enqueues the `message` to be sent and returns immediately.
    *
    * If any underlying buffers are full, they may be removed and a warning will
    * be issued.
    */
  def send(message: Either[InternalLogMessage, WSLogMessage]): Unit
}
