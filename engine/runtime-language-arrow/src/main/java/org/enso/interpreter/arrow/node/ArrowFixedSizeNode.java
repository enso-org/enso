package org.enso.interpreter.arrow.node;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.arrow.LogicalLayout;
import org.enso.interpreter.arrow.runtime.ArrowFixedSizeArrayFactory;

public class ArrowFixedSizeNode extends Node {

  static ArrowFixedSizeNode create() {
    return new ArrowFixedSizeNode();
  }

  public Object execute(LogicalLayout layoutType) {
    return new ArrowFixedSizeArrayFactory(layoutType);
  }
}
