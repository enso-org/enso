package org.enso.interpreter.node.expression.builtin.meta;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

@BuiltinType
public class ProjectDescription extends UniquelyConstructibleBuiltin {
  @Override
  protected String getConstructorName() {
    return "Value";
  }

  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("root_path", "ns", "n");
  }
}
