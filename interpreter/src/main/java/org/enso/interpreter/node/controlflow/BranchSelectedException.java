package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.nodes.ControlFlowException;

public class BranchSelectedException extends ControlFlowException {
  private final Object result;

  public BranchSelectedException(Object result) {
    this.result = result;
  }

  public Object getResult() {
    return result;
  }
}
