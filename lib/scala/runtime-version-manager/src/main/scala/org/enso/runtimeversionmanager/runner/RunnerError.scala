package org.enso.runtimeversionmanager.runner

/** Represents an error encountered when running the component. */
case class RunnerError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {

  /** @inheritdoc */
  override def toString: String = message
}
