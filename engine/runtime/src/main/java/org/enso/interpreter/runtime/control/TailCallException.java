package org.enso.interpreter.runtime.control;

import com.oracle.truffle.api.nodes.ControlFlowException;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * Used to model the switch of control-flow from standard stack-based execution to looping.
 *
 * <p>This is used as part of the tail-call optimisation functionality in the interpreter.
 */
public class TailCallException extends ControlFlowException {
  private final Function function;
  private final CallerInfo callerInfo;
  private final Object state;
  private final Object[] arguments;

  /**
   * Creates a new exception containing the necessary data to continue computation.
   *
   * @param function the function to execute in a loop
   * @param state the state to pass to the function
   * @param arguments the arguments to {@code function}
   */
  public TailCallException(
      Function function, CallerInfo callerInfo, Object state, Object[] arguments) {
    this.function = function;
    this.callerInfo = callerInfo;
    this.arguments = arguments;
    this.state = state;
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
   * Gets the state to pass to the function.
   *
   * @return the state to pass for next call
   */
  public Object getState() {
    return state;
  }

  /**
   * Gets the caller info to pass to the function.
   *
   * @return the state to pass for next call
   */
  public CallerInfo getCallerInfo() {
    return callerInfo;
  }
}
