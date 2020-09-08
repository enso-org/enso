package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(type = "Array", name = "new", description = "Creates an empty array.")
public class NewNode extends Node {

  Object execute(Object _this, long size) {
    return new Vector(size);
  }
}
