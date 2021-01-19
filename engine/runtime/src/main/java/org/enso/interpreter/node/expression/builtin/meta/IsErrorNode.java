package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Meta",
    name = "is_error",
    description = "Checks if the argument is an error.")
public class IsErrorNode extends Node {
  boolean execute(Object _this, Object value) {
    return TypesGen.isDataflowError(value);
  }
}
