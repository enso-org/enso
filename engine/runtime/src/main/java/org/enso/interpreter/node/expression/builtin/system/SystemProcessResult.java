package org.enso.interpreter.node.expression.builtin.system;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

@BuiltinType
public class SystemProcessResult extends UniquelyConstructibleBuiltin {
  @Override
  protected String getConstructorName() {
    return "Result";
  }

  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("exit_code", "stdout", "stderr");
  }
}
