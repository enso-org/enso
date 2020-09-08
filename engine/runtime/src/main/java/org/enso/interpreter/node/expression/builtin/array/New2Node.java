package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(type = "Array", name = "new_2", description = "Creates an empty array.")
public class New2Node extends Node {

  Object execute(Object _this, Object item_1, Object item_2) {
    return new Vector(item_1, item_2);
  }
}
