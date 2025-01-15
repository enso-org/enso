package org.enso.interpreter.node.expression.builtin.error;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;

@BuiltinType(name = "Standard.Base.Data.Vector.No_Wrap")
public class NoWrap extends UniquelyConstructibleBuiltin {

  @Override
  protected String getConstructorName() {
    return "Value";
  }

  @Override
  protected List<String> getConstructorParamNames() {
    return List.of();
  }
}
