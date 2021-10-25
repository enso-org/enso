package org.enso.loggingservice

case class LoggingServiceAlreadyInitializedException()
    extends RuntimeException(
      "The logging service was already initialized. " +
      "If it should be restarted, it should be torn down first."
    )
