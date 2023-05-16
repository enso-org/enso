package org.enso.interpreter.node.expression.builtin.function;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Function.Function")
public class Function extends Builtin {
  @Override
  public boolean containsValues() {
    return true;
  }
}
