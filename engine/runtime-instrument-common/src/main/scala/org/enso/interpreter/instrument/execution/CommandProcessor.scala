package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.command.Command

/** Defines a uniform interface to execute commands.
  */
trait CommandProcessor {

  /** Invokes a command with the provided context.
    *
    * @param cmd a command to execute
    * @return if the command is asynchronous, a future signaling the completion of computations,
    *         a signal indicating the completion otherwise
    */
  def invoke(cmd: Command): cmd.Result[Completion]

  /** Stops the command processor.
    */
  def stop(): Unit

}
