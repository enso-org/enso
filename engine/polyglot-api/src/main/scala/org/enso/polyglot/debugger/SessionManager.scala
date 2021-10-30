package org.enso.polyglot.debugger

/** Interface for executing Repl commands inside of a Repl session.
  *
  * A single instance is valid only during the current session, it is provided
  * to the SessionManager on start of each session.
  */
trait ReplExecutor {

  /** Evaluates an arbitrary expression in the current execution context.
    *
    * @param expression the expression to evaluate
    * @return the result of evaluating the expression or error
    */
  def evaluate(
    expression: String
  ): Either[Exception, ObjectRepresentation]

  /** Lists all the bindings available in the current execution scope.
    *
    * @return a map, where keys are variable names and values are current
    *         values of variables.
    */
  def listBindings(): Map[String, ObjectRepresentation]

  /** Terminates this REPL session.
    *
    * The last result of [[evaluate]] (or `Unit` if [[evaluate]] was not called
    * before) will be returned from the instrumented node.
    *
    * This function must always be called at the end of REPL session, as
    * otherwise the program will never resume. It's forbidden to use this object
    * after exit has been called.
    *
    * As it brings control back to the interpreter, it never returns.
    */
  def exit(): Nothing
}

/** Trait that should be implemented by Repl users to define how to handle Repl
  * sessions.
  */
trait SessionManager {

  /** Method that is run when starting each Repl session. The whole session
    * lives inside this method. It should always be finished by running
    * `executor.exit()`.
    *
    * @param executor the interface for sending commands to the Repl during the
    *                 session
    * @return does not return as it has to be ended by a call to
    *         `executor.exit()` which brings control back to the interpreter.
    */
  def startSession(executor: ReplExecutor): Nothing
}
