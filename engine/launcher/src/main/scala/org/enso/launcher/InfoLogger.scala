package org.enso.launcher

import com.typesafe.scalalogging.Logger

/**
  * This is a temporary object that should be at some point replaced with the
  * actual logging service.
  *
  * TODO [RW] this should be replaced with the proper logging service once it
  *  is implemented in #1031
  */
object InfoLogger {

//  private val logger = Logger("launcher")

  /**
    * Logs an info level message.
    */
  def info(msg: => String): Unit = {
    // TODO [RW] add special handling, rename etc.
    println(msg)
  }

}
