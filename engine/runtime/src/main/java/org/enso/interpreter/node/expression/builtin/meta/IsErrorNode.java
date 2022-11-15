package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Meta",
    name = "is_error",
    description = "Checks if the argument is an error.",
    autoRegister = false)
public class IsErrorNode extends Node {
  boolean execute(@AcceptsError Object value) {
    return TypesGen.isDataflowError(value);
  }
}
