package org.enso.interpreter.node.expression.builtin.ordering;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType
public final class DefaultComparator extends Builtin {
  public DefaultComparator() {
    super(Object.class);
  }
}
