package org.enso.interpreter.node.expression.builtin.number;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Data.Numbers.Nan")
public class Nan extends Builtin {
  @Override
  protected Class<? extends Builtin> getSuperType() {
    return Decimal.class;
  }

  @Override
  protected boolean containsValues() {
    return true;
  }
}
