package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;

@BuiltinMethod(
    type = "Meta",
    name = "type_of",
    description = "Returns the type of a value.",
    autoRegister = false)
@GenerateUncached
public final class TypeOfBuiltin extends Node {
  private @Child TypeOfNode typeOf = TypeOfNode.create();

  private TypeOfBuiltin() {}

  public Object execute(@AcceptsError Object value) {
    return typeOf.execute(value);
  }

  public static TypeOfBuiltin build() {
    return new TypeOfBuiltin();
  }
}
