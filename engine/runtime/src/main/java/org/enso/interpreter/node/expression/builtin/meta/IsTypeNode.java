package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Type;

@BuiltinMethod(
    type = "Meta",
    name = "is_type",
    description = "Checks if the argument is a type",
    autoRegister = false)
public class IsTypeNode extends Node {
  boolean execute(@AcceptsError Object value) {
    return value instanceof Type;
  }
}
