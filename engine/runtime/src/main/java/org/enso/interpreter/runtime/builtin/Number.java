package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.number.BigInteger;
import org.enso.interpreter.node.expression.builtin.number.Decimal;
import org.enso.interpreter.node.expression.builtin.number.Integer;
import org.enso.interpreter.node.expression.builtin.number.SmallInteger;
import org.enso.interpreter.node.expression.builtin.ordering.Less;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container for all number-related builtins. */
public class Number {
  private final BuiltinAtomConstructor smallInteger;
  private final BuiltinAtomConstructor bigInteger;
  private final BuiltinAtomConstructor integer;
  private final BuiltinAtomConstructor number;
  private final BuiltinAtomConstructor decimal;

  /** Creates builders for number Atom Constructors. */
  public Number(Builtins builtins) {
    smallInteger = new BuiltinAtomConstructor(builtins, SmallInteger.class);
    bigInteger = new BuiltinAtomConstructor(builtins, BigInteger.class);
    integer = new BuiltinAtomConstructor(builtins, Integer.class);
    number =
        new BuiltinAtomConstructor(
            builtins, org.enso.interpreter.node.expression.builtin.number.Number.class);
    decimal = new BuiltinAtomConstructor(builtins, Decimal.class);
  }

  /** @return the Int64 atom constructor. */
  public AtomConstructor getSmallInteger() {
    return smallInteger.constructor();
  }

  /** @return the Big_Integer atom constructor. */
  public AtomConstructor getBigInteger() {
    return bigInteger.constructor();
  }

  /** @return the Integer atom constructor */
  public AtomConstructor getInteger() {
    return integer.constructor();
  }

  /** @return the Number atom constructor */
  public AtomConstructor getNumber() {
    return number.constructor();
  }

  /** @return the Decimal atom constructor */
  public AtomConstructor getDecimal() {
    return decimal.constructor();
  }
}
