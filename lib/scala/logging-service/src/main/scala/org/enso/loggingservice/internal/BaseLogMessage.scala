package org.enso.loggingservice.internal

import java.time.Instant

import org.enso.loggingservice.LogLevel

/**
  * A base type for log messages parametrized by the exception representation.
  */
trait BaseLogMessage[ExceptionType] {
  def level:     LogLevel
  def timestamp: Instant
  def group:     String
  def message:   String
  def exception: Option[ExceptionType]
}
