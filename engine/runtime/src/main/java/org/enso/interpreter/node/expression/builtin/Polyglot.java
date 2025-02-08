package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;

@BuiltinType
public final class Polyglot extends Builtin {
  public Polyglot() {
    super(Object.class);
  }
}
