package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(type = "Array", name = "empty", description = "Creates an empty array.")
public class EmptyNode extends Node {

  Object execute(Object _this) {
    return new Array();
  }
}
