package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Array",
    name = "new_1",
    description = "Creates an array with one given element.")
public class New1Node extends Node {
  Object execute(Object _this, Object item_1) {
    return new Array(item_1);
  }
}
