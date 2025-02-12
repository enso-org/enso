package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinType(name = "Standard.Base.Panic.Panic")
public final class Panic extends Builtin {
  public Panic() {
    super(PanicException.class);
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
