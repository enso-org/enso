package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Array",
    name = "at",
    description = "Gets an array element at the given index.")
public class GetAtNode extends Node {

  Object execute(Array _this, long index) {
    return _this.getItems()[(int) index];
  }
}
