package org.enso.interpreter.node.expression.builtin.number;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType
public class BigInteger extends Builtin {
  @Override
  protected Class<? extends Builtin> getSuperType() {
    return Integer.class;
  }
}
