package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.nodes.Node;

public abstract class ForeignFunctionCallNode extends Node {
  public abstract Object execute(Object[] arguments);

}
