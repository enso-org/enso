package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.command.Command

import scala.concurrent.Future

/** Defines a uniform interface to execute commands.
  */
trait CommandProcessor {

  /** Invokes a command with the provided context.
    *
    * @param cmd a command to execute
    * @return a future signaling the completion of computations
    */
  def invoke(cmd: Command): Future[Completion]

  /** Stops the command processor.
    */
  def stop(): Unit

}
