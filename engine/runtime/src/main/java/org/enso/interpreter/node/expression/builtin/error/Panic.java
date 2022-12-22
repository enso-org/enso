package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Panic.Panic")
public class Panic extends Builtin {
  @Override
  protected boolean containsValues() {
    return true;
  }
}
