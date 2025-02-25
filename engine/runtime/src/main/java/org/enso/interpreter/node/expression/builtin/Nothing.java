package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;

@BuiltinType(name = "Standard.Base.Nothing.Nothing")
public final class Nothing extends Builtin {
  public Nothing() {
    super(Void.class);
  }

  @Override
  public boolean containsValues() {
    return false;
  }
}
