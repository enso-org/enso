package org.enso.interpreter.arrow.node;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.arrow.ArrowParser;
import org.enso.interpreter.arrow.runtime.ArrowFixedSizeArrayFactory;

public class ArrowFixedSizeNode extends Node {

  static ArrowFixedSizeNode build() {
    return new ArrowFixedSizeNode();
  }

  public Object execute(ArrowParser.LogicalLayout layoutType) {
    return new ArrowFixedSizeArrayFactory(layoutType);
  }
}
