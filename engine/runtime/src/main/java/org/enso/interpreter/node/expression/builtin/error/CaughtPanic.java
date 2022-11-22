package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

import java.util.List;

@BuiltinType
public class CaughtPanic extends UniquelyConstructibleBuiltin {
  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("payload", "internal_original_exception");
  }
}
