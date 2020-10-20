package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Array",
    name = "new_2",
    description = "Creates an array with two given elements.")
public class New2Node extends Node {

  Object execute(Object _this, Object item_1, Object item_2) {
    return new Array(item_1, item_2);
  }
}
