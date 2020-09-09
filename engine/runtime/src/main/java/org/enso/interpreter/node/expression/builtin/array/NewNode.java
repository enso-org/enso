package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Array",
    name = "new",
    description = "Creates an uninitialized array of a given size.")
public class NewNode extends Node {

  Object execute(Object _this, long size) {
    return new Array(size);
  }
}
