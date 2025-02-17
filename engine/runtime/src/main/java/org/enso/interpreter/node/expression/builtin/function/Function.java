package org.enso.interpreter.node.expression.builtin.function;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Function.Function")
public final class Function extends Builtin {
  public Function() {
    super(org.enso.interpreter.runtime.callable.function.Function.class);
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
