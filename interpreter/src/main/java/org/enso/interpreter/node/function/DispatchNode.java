package org.enso.interpreter.node.function;

import com.oracle.truffle.api.nodes.Node;

public abstract class DispatchNode extends Node {
  public abstract Object executeDispatch(Object receiver, Object[] arguments);
}
