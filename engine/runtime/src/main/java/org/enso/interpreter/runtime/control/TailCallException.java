package org.enso.interpreter.runtime.control;

import com.oracle.truffle.api.nodes.ControlFlowException;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.Warning;

/**
 * Used to model the switch of control-flow from standard stack-based execution to looping.
 *
 * <p>This is used as part of the tail-call optimisation functionality in the interpreter.
 */
public class TailCallException extends ControlFlowException {
  private final Function function;
  private final CallerInfo callerInfo;
  private final Object[] arguments;

  private final Warning[] warnings;

  /**
   * Creates a new exception containing the necessary data to continue computation.
   *
   * @param function the function to execute in a loop
   * @param state the state to pass to the function
   * @param arguments the arguments to {@code function}
   */
  public TailCallException(Function function, CallerInfo callerInfo, Object[] arguments) {
    this.function = function;
    this.callerInfo = callerInfo;
    this.arguments = arguments;
    this.warnings = null;
  }

  /**
   * Creates a new tail exception from the original one and attach warnings.
   *
   * @param e the original TailCallException to be propagated
   * @param warnings warnings to be associated with the given exception
   */
  public TailCallException(TailCallException e, Warning[] warnings) {
    assert e.getWarnings() == null;
    this.function = e.getFunction();
    this.callerInfo = e.getCallerInfo();
    this.arguments = e.getArguments();
    this.warnings = warnings;
  }

  /**
   * Gets the function to execute.
   *
   * @return the {@link Function} awaiting execution
   */
  public Function getFunction() {
    return function;
  }

  /**
   * Gets the arguments for the function.
   *
   * @return the arguments for the associated {@link Function}
   */
  public Object[] getArguments() {
    return arguments;
  }

  /**
   * Gets the caller info to pass to the function.
   *
   * @return the state to pass for next call
   */
  public CallerInfo getCallerInfo() {
    return callerInfo;
  }

  /**
   * Returns warnings that have been extracted before the function was invoked with the given
   * arguments.
   *
   * @return warnings extracted from the expression or null, if none were found
   */
  public Warning[] getWarnings() {
    return warnings;
  }
}
