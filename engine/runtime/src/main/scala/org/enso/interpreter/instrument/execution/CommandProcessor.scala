package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.command.Command
import org.enso.interpreter.instrument.execution.CommandProcessor.Done

import scala.concurrent.Future

/**
  * Defines a uniform interface to execute commands.
  */
trait CommandProcessor {

  /**
    * Invokes a command with the provided context.
    *
    * @param cmd a command to execute
    * @param ctx contains suppliers of services to perform a request
    * @return a future signaling the completion of computations
    */
  def invoke(cmd: Command, ctx: RuntimeContext): Future[Done.type]

}

object CommandProcessor {

  /**
    * Signals completion of computations.
    */
  case object Done

}
