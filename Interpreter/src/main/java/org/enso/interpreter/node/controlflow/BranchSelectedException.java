package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.nodes.ControlFlowException;

/** This exception is used to signal when a certain branch in a case expression has been taken. */
public class BranchSelectedException extends ControlFlowException {
  private final Object result;

  /**
   * Creates a new exception instance.
   *
   * @param result the result of executing the branch this is thrown from
   */
  public BranchSelectedException(Object result) {
    this.result = result;
  }

  /**
   * Gets the result of executing the case branch in question.
   *
   * @return the result of executing the case branch from which this is thrown
   */
  public Object getResult() {
    return result;
  }
}
