package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(type = "Array", name = "new_1", description = "Creates an empty array.")
public class New1Node extends Node {

  Object execute(Object _this, Object item_1) {
    return new Vector(item_1);
  }
}
