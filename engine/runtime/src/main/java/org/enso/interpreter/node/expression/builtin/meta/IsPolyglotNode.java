package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Meta",
    name = "is_polyglot",
    description = "Checks if the argument is a polyglot value.",
    autoRegister = false)
public abstract class IsPolyglotNode extends Node {
  static IsPolyglotNode build() {
    return IsPolyglotNodeGen.create();
  }

  abstract boolean execute(@AcceptsError Object value);

  @Specialization
  boolean doExecute(Object value) {
    return EnsoContext.get(this).getEnvironment().isHostObject(value);
  }
}
