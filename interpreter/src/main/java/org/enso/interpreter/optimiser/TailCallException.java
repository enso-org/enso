package org.enso.interpreter.optimiser;

import com.oracle.truffle.api.nodes.ControlFlowException;
import org.enso.interpreter.runtime.Function;

public class TailCallException extends ControlFlowException {
  private final Function function;
  private final Object[] arguments;

  public TailCallException(Function function, Object[] arguments) {
    this.function = function;
    this.arguments = arguments;
  }

  public Function getFunction() {
    return function;
  }

  public Object[] getArguments() {
    return arguments;
  }
}
