package org.enso.interpreter.node.expression.builtin.number;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Data.Numbers.Decimal")
public class Decimal extends Builtin {
  @Override
  protected Class<? extends Builtin> getSuperType() {
    return Number.class;
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
