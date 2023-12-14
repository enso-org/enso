package org.enso.interpreter.arrow.node;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.arrow.ArrowParser;
import org.enso.interpreter.arrow.runtime.ArrowCastToFixedSizeArrayFactory;

public class ArrowCastFixedSizeNode extends Node {

  static ArrowCastFixedSizeNode build() {
    return new ArrowCastFixedSizeNode();
  }

  public Object execute(ArrowParser.LogicalLayout layoutType) {
    return new ArrowCastToFixedSizeArrayFactory(layoutType);
  }
}
