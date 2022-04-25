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
  @CompilerDirectives.CompilationFinal
  private AtomConstructor smallInteger;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor bigInteger;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor integer;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor number;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor decimal;

  private final Builtins builtins;

  /**
   * Creates and registers number builtins.
   *
   * @param language the current language instance.
   * @param scope the builtins scope.
   */
  public Number(Builtins builtins) {
    this.builtins = builtins;
  }

  /** @return the Int64 atom constructor. */
  public AtomConstructor getSmallInteger() {
    if (smallInteger == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      smallInteger = builtins.getBuiltinType(SmallInteger.class);
    }
    return smallInteger;
  }

  /** @return the Big_Integer atom constructor. */
  public AtomConstructor getBigInteger() {
    if (bigInteger == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      bigInteger = builtins.getBuiltinType(BigInteger.class);
    }
    return bigInteger;
  }

  /** @return the Integer atom constructor */
  public AtomConstructor getInteger() {
    if (integer == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      integer = builtins.getBuiltinType(Integer.class);
    }
    return integer;
  }

  /** @return the Number atom constructor */
  public AtomConstructor getNumber() {
    if (number == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      number = builtins.getBuiltinType(org.enso.interpreter.node.expression.builtin.number.Number.class);
    }
    return number;
  }

  /** @return the Decimal atom constructor */
  public AtomConstructor getDecimal() {
    if (decimal == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      decimal = builtins.getBuiltinType(Decimal.class);
    }
    return decimal;
  }
}
