package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

import java.util.List;

@BuiltinType
public class ProjectDescription extends UniquelyConstructibleBuiltin {
  @Override
  protected String getConstructorName() {
    return "Value";
  }

  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("prim_root_file", "prim_config");
  }
}
