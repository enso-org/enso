package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(
    type = "Meta",
    name = "is_polyglot",
    description = "Checks if the argument is a polyglot value.")
public abstract class IsPolyglotNode extends Node {
  static IsPolyglotNode build() {
    return IsPolyglotNodeGen.create();
  }

  abstract boolean execute(Object _this, @AcceptsError Object value);

  @Specialization
  boolean doExecute(Object _this, Object value) {
    return Context.get(this).getEnvironment().isHostObject(value);
  }
}
