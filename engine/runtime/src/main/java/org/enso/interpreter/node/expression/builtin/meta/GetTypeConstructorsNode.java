package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;

@BuiltinMethod(
    type = "Meta",
    name = "get_type_constructors",
    description = "Gets the constructors of a type.",
    autoRegister = false)
public abstract class GetTypeConstructorsNode extends Node {
  static GetTypeConstructorsNode build() {
    return GetTypeConstructorsNodeGen.create();
  }

  abstract Array execute(Object type);

  @Specialization
  Array doStruct(Type type) {
    return new Array(type.getConstructors().values().toArray());
  }

  @Fallback
  Array empty(Object type) {
    return Array.empty();
  }
}
