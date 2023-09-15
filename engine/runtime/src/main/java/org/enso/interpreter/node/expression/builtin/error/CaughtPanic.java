package org.enso.interpreter.node.expression.builtin.error;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

@BuiltinType
public class CaughtPanic extends UniquelyConstructibleBuiltin {
  @Override
  protected String getConstructorName() {
    return "Panic";
  }

  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("payload", "internal_original_exception");
  }
}
