package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(type = "Array", name = "empty", description = "Creates an empty array.")
public class LengthNode extends Node {

  long execute(Vector _this) {
    return _this.getItems().length;
  }
}
