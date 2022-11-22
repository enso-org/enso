package org.enso.interpreter.node.expression.builtin.system;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

import java.util.List;

@BuiltinType
public class SystemProcessResult extends UniquelyConstructibleBuiltin {
  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("exit_code", "stdout", "stderr");
  }
}
