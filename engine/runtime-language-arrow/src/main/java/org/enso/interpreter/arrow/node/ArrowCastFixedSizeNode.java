package org.enso.interpreter.arrow.node;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.arrow.LogicalLayout;
import org.enso.interpreter.arrow.runtime.ArrowCastToFixedSizeArrayFactory;

public class ArrowCastFixedSizeNode extends Node {

  static ArrowCastFixedSizeNode create() {
    return new ArrowCastFixedSizeNode();
  }

  public Object execute(LogicalLayout layoutType) {
    return new ArrowCastToFixedSizeArrayFactory(layoutType);
  }
}
