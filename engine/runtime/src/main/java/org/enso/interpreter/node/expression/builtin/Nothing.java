package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;

@BuiltinType(name = "Standard.Base.Nothing.Nothing")
public class Nothing extends Builtin {
  @Override
  public boolean containsValues() {
    return false;
  }
}
