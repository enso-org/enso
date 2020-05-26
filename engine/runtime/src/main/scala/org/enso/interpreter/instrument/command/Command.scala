package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext

/**
  * Base command trait that encapsulates a function request. Uses
  * [[RuntimeContext]] to perform a request.
  */
trait Command {

  /**
    * Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  def execute(implicit ctx: RuntimeContext): Unit

}
