package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.node.expression.builtin.number.Float;
import org.enso.interpreter.node.expression.builtin.number.Integer;
import org.enso.interpreter.runtime.data.Type;

/** A container for all number-related builtins. */
public class Number {
  private final Builtin integer;
  private final Builtin number;
  private final Builtin ensoFloat;

  /** Creates builders for number Atom Constructors. */
  public Number(Builtins builtins) {
    integer = builtins.getBuiltinType(Integer.class);
    number =
        builtins.getBuiltinType(org.enso.interpreter.node.expression.builtin.number.Number.class);
    ensoFloat = builtins.getBuiltinType(Float.class);
  }

  /** @return the Integer atom constructor */
  public Type getInteger() {
    return integer.getType();
  }

  /** @return the Number atom constructor */
  public Type getNumber() {
    return number.getType();
  }

  /** @return the Decimal atom constructor */
  public Type getFloat() {
    return ensoFloat.getType();
  }
}
