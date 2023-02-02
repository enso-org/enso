package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;

@BuiltinType(name = "Standard.Base.Any.Any")
public class Any extends Builtin {
  @Override
  public Class<? extends Builtin> getSuperType() {
    return null;
  }

  @Override
  protected boolean containsValues() {
    return true;
  }
}
