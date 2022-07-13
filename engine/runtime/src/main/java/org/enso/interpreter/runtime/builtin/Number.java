package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.number.BigInteger;
import org.enso.interpreter.node.expression.builtin.number.Decimal;
import org.enso.interpreter.node.expression.builtin.number.Integer;
import org.enso.interpreter.node.expression.builtin.number.SmallInteger;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** A container for all number-related builtins. */
public class Number {
  private final BuiltinType smallInteger;
  private final BuiltinType bigInteger;
  private final BuiltinType integer;
  private final BuiltinType number;
  private final BuiltinType decimal;

  /** Creates builders for number Atom Constructors. */
  public Number(Builtins builtins) {
    smallInteger = new BuiltinType(builtins, SmallInteger.class);
    bigInteger = new BuiltinType(builtins, BigInteger.class);
    integer = new BuiltinType(builtins, Integer.class);
    number =
        new BuiltinType(
            builtins, org.enso.interpreter.node.expression.builtin.number.Number.class);
    decimal = new BuiltinType(builtins, Decimal.class);
  }

  /** @return the Int64 atom constructor. */
  public AtomConstructor getSmallInteger() {
    return smallInteger.getType();
  }

  /** @return the Big_Integer atom constructor. */
  public AtomConstructor getBigInteger() {
    return bigInteger.getType();
  }

  /** @return the Integer atom constructor */
  public AtomConstructor getInteger() {
    return integer.getType();
  }

  /** @return the Number atom constructor */
  public AtomConstructor getNumber() {
    return number.getType();
  }

  /** @return the Decimal atom constructor */
  public AtomConstructor getDecimal() {
    return decimal.getType();
  }
}
