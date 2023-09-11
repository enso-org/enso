package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.node.expression.builtin.number.Decimal;
import org.enso.interpreter.node.expression.builtin.number.Integer;
import org.enso.interpreter.runtime.data.Type;

/** A container for all number-related builtins. */
public class Number {
  private final Builtin integer;
  private final Builtin number;
  private final Builtin decimal;

  /** Creates builders for number Atom Constructors. */
  public Number(Builtins builtins) {
    integer = builtins.getBuiltinType(Integer.class);
    number =
        builtins.getBuiltinType(org.enso.interpreter.node.expression.builtin.number.Number.class);
    decimal = builtins.getBuiltinType(Decimal.class);
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
  public Type getDecimal() {
    return decimal.getType();
  }
}
