package org.enso.loggingservice.client

import org.slf4j.{Logger, Marker}

class WSLogger(name: String) extends Logger {
  override def getName: String = name

  override def isTraceEnabled: Boolean = ???

  override def trace(msg: String): Unit = ???

  override def trace(format: String, arg: Object): Unit = ???

  override def trace(format: String, arg1: Object, arg2: Object): Unit = ???

  override def trace(format: String, arguments: Object*): Unit = ???

  override def trace(msg: String, t: Throwable): Unit = ???

  override def isTraceEnabled(marker: Marker): Boolean = ???

  override def trace(marker: Marker, msg: String): Unit = ???

  override def trace(marker: Marker, format: String, arg: Object): Unit = ???

  override def trace(
    marker: Marker,
    format: String,
    arg1: Object,
    arg2: Object
  ): Unit = ???

  override def trace(marker: Marker, format: String, argArray: Object*): Unit =
    ???

  override def trace(marker: Marker, msg: String, t: Throwable): Unit = ???

  override def isDebugEnabled: Boolean = ???

  override def debug(msg: String): Unit = ???

  override def debug(format: String, arg: Object): Unit = ???

  override def debug(format: String, arg1: Object, arg2: Object): Unit = ???

  override def debug(format: String, arguments: Object*): Unit = ???

  override def debug(msg: String, t: Throwable): Unit = ???

  override def isDebugEnabled(marker: Marker): Boolean = ???

  override def debug(marker: Marker, msg: String): Unit = ???

  override def debug(marker: Marker, format: String, arg: Object): Unit = ???

  override def debug(
    marker: Marker,
    format: String,
    arg1: Object,
    arg2: Object
  ): Unit = ???

  override def debug(marker: Marker, format: String, arguments: Object*): Unit =
    ???

  override def debug(marker: Marker, msg: String, t: Throwable): Unit = ???

  override def isInfoEnabled: Boolean = ???

  override def info(msg: String): Unit = ???

  override def info(format: String, arg: Object): Unit = ???

  override def info(format: String, arg1: Object, arg2: Object): Unit = ???

  override def info(format: String, arguments: Object*): Unit = ???

  override def info(msg: String, t: Throwable): Unit = ???

  override def isInfoEnabled(marker: Marker): Boolean = ???

  override def info(marker: Marker, msg: String): Unit = ???

  override def info(marker: Marker, format: String, arg: Object): Unit = ???

  override def info(
    marker: Marker,
    format: String,
    arg1: Object,
    arg2: Object
  ): Unit = ???

  override def info(marker: Marker, format: String, arguments: Object*): Unit =
    ???

  override def info(marker: Marker, msg: String, t: Throwable): Unit = ???

  override def isWarnEnabled: Boolean = ???

  override def warn(msg: String): Unit = ???

  override def warn(format: String, arg: Object): Unit = ???

  override def warn(format: String, arguments: Object*): Unit = ???

  override def warn(format: String, arg1: Object, arg2: Object): Unit = ???

  override def warn(msg: String, t: Throwable): Unit = ???

  override def isWarnEnabled(marker: Marker): Boolean = ???

  override def warn(marker: Marker, msg: String): Unit = ???

  override def warn(marker: Marker, format: String, arg: Object): Unit = ???

  override def warn(
    marker: Marker,
    format: String,
    arg1: Object,
    arg2: Object
  ): Unit = ???

  override def warn(marker: Marker, format: String, arguments: Object*): Unit =
    ???

  override def warn(marker: Marker, msg: String, t: Throwable): Unit = ???

  override def isErrorEnabled: Boolean = ???

  override def error(msg: String): Unit = ???

  override def error(format: String, arg: Object): Unit = ???

  override def error(format: String, arg1: Object, arg2: Object): Unit = ???

  override def error(format: String, arguments: Object*): Unit = ???

  override def error(msg: String, t: Throwable): Unit = ???

  override def isErrorEnabled(marker: Marker): Boolean = ???

  override def error(marker: Marker, msg: String): Unit = ???

  override def error(marker: Marker, format: String, arg: Object): Unit = ???

  override def error(
    marker: Marker,
    format: String,
    arg1: Object,
    arg2: Object
  ): Unit = ???

  override def error(marker: Marker, format: String, arguments: Object*): Unit =
    ???

  override def error(marker: Marker, msg: String, t: Throwable): Unit = ???
}
