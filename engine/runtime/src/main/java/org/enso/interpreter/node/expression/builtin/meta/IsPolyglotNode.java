package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Meta",
    name = "is_polyglot",
    description = "Checks if the argument is a polyglot value.")
public abstract class IsPolyglotNode extends Node {
  static IsPolyglotNode build() {
    return IsPolyglotNodeGen.create();
  }

  abstract boolean execute(Object _this, Object value);

  @Specialization
  boolean doExecute(Object _this, Object value, @CachedContext(Language.class) Context context) {
    return context.getEnvironment().isHostObject(value);
  }
}
