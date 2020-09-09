package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(type = "Array", name = "length", description = "Returns the length of an array.")
public class LengthNode extends Node {

  long execute(Array _this) {
    return _this.getItems().length;
  }
}
