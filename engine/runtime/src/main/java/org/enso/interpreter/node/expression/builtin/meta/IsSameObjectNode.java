package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Meta",
    name = "is_same_object",
    description = "Checks if the two arguments share an underlying reference.")
public class IsSameObjectNode extends Node {
  boolean execute(@AcceptsError Object value_1, @AcceptsError Object value_2) {
    return value_1 == value_2;
  }
}
