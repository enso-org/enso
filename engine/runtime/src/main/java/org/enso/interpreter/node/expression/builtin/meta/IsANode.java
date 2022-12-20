package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Meta",
    name = "is_a",
    description = "Checks type of a node.",
    autoRegister = false)
public class IsANode extends Node {
  private @Child IsValueOfTypeNode check = IsValueOfTypeNode.build();

  public boolean execute(@AcceptsError Object value, Object type) {
    return check.execute(type, value);
  }
}
