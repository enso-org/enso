package org.enso.loggingservice.internal.service

/**
  * A service backend that is used to process incoming log messages.
  */
trait Service {

  /**
    * Terminates the service and releases its resources.
    */
  def terminate(): Unit = {}
}
