package org.enso.interpreter.epb;

import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.nodes.Node;

/** An interface for nodes responsible for calling into foreign languages. */
abstract class ForeignFunctionCallNode extends Node {
  /**
   * Executes the foreign call.
   *
   * @param arguments the arguments to pass to the foreign function
   * @return the result of executing the foreign function
   * @throws InteropException when execution fails during foreign call
   */
  public abstract Object execute(Object[] arguments) throws InteropException;
}
