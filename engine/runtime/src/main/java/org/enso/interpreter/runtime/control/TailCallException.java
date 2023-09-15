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
   * @param callerInfo the caller execution context
   * @param arguments the arguments to {@code function}
   */
  public TailCallException(Function function, CallerInfo callerInfo, Object[] arguments) {
    this.function = function;
    this.callerInfo = callerInfo;
    this.arguments = arguments;
    this.warnings = null;
  }

  private TailCallException(
      Function function, CallerInfo callerInfo, Object[] arguments, Warning[] warnings) {
    this.function = function;
    this.callerInfo = callerInfo;
    this.arguments = arguments;
    this.warnings = warnings;
  }

  /**
   * Creates a new exception containing the necessary data to continue computation.
   *
   * @param origin the original tail call exception
   * @param warnings warnings to be associated with the tail call exception
   */
  public TailCallException(TailCallException origin, Warning[] warnings) {
    this(origin.getFunction(), origin.getCallerInfo(), origin.getArguments(), warnings);
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
   * Gets the warnings that should be appended to the result of calling the function.
   *
   * @return the warnings to be appended to the result of the call, or null if empty
   */
  public Warning[] getWarnings() {
    return warnings;
  }
}
