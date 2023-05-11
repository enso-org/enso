package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;

@BuiltinType(name = "Standard.Base.Error.Error")
public class Error extends Builtin {
  @Override
  protected Class<? extends Builtin> getSuperType() {
    return null;
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
