package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Any", name = "==", description = "Generic equality.")
public class ObjectEqualsNode extends Node {
  boolean execute(Object _this, Object that) {
    return _this.equals(that);
  }
}
