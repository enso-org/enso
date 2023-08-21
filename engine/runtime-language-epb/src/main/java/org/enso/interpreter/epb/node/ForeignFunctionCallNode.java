package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;

/** An interface for nodes responsible for calling into foreign languages. */
public abstract class ForeignFunctionCallNode extends Node {
  /**
   * Executes the foreign call.
   *
   * @param arguments the arguments to pass to the foreign function
   * @return the result of executing the foreign function
   */
  public abstract Object execute(Object[] arguments)
      throws UnsupportedTypeException, ArityException, UnsupportedMessageException;
}
