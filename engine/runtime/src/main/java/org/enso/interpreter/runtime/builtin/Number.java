package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container for all number-related builtins. */
public class Number {
  private final AtomConstructor smallInteger;
  private final AtomConstructor bigInteger;
  private final AtomConstructor integer;
  private final AtomConstructor number;
  private final AtomConstructor decimal;

  /**
   * Creates and registers number builtins.
   *
   * @param language the current language instance.
   * @param scope the builtins scope.
   */
  public Number(Language language, ModuleScope scope) {
    number = new AtomConstructor("Number", scope).initializeFields();
    smallInteger = new AtomConstructor("Small_Integer", scope).initializeFields();
    integer = new AtomConstructor("Integer", scope).initializeFields();
    bigInteger = new AtomConstructor("Big_Integer", scope).initializeFields();
    decimal = new AtomConstructor("Decimal", scope).initializeFields();

    registerInt64Methods(language, scope);
    registerBigIntegerMethods(language, scope);
    registerDecimalMethods(language, scope);

    scope.registerConstructor(number);
    scope.registerConstructor(smallInteger);
    scope.registerConstructor(integer);
    scope.registerConstructor(bigInteger);
    scope.registerConstructor(decimal);
  }

  private void registerInt64Methods(Language language, ModuleScope scope) {
    scope.registerMethod(
        smallInteger,
        "+",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.AddMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "-",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.SubtractMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "*",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.MultiplyMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "^",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.PowMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "/",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.DivideMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "div",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.DivMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "%",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.ModMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "negate",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.NegateMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "abs",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.AbsMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "==",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.EqualsMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        ">",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.GreaterMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        ">=",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.GreaterOrEqualMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "<",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.LessMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "<=",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.LessOrEqualMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "to_decimal",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.ToDecimalMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "floor",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.FloorMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "ceil",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.CeilMethodGen.makeFunction(
            language));
    scope.registerMethod(
        smallInteger,
        "bit_and",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitAndMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "bit_or",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitOrMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "bit_xor",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitXorMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "bit_not",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitNotMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "bit_shift",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitShiftMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "bit_shift_l",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitShiftMethodGen
            .makeFunction(language));
    scope.registerMethod(
        smallInteger,
        "bit_shift_r",
        org.enso.interpreter.node.expression.builtin.number.smallInteger.BitShiftRightMethodGen
            .makeFunction(language));
  }

  private void registerBigIntegerMethods(Language language, ModuleScope scope) {

    scope.registerMethod(
        bigInteger,
        "+",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.AddMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "-",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.SubtractMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "*",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.MultiplyMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "^",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.PowMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "/",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.DivideMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "div",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.DivMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "%",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.ModMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "negate",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.NegateMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "abs",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.AbsMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "==",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.EqualsMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        ">",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.GreaterMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        ">=",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.GreaterOrEqualMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "<",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.LessMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "<=",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.LessOrEqualMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "to_decimal",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.ToDecimalMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "floor",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.FloorMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "ceil",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.CeilMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "bit_and",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitAndMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "bit_or",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitOrMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "bit_xor",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitXorMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "bit_not",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitNotMethodGen.makeFunction(
            language));
    scope.registerMethod(
        bigInteger,
        "bit_shift",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitShiftMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "bit_shift_l",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitShiftMethodGen
            .makeFunction(language));
    scope.registerMethod(
        bigInteger,
        "bit_shift_r",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.BitShiftRightMethodGen
            .makeFunction(language));
  }

  private void registerDecimalMethods(Language language, ModuleScope scope) {

    scope.registerMethod(
        decimal,
        "+",
        org.enso.interpreter.node.expression.builtin.number.decimal.AddMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "-",
        org.enso.interpreter.node.expression.builtin.number.decimal.SubtractMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "*",
        org.enso.interpreter.node.expression.builtin.number.decimal.MultiplyMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "^",
        org.enso.interpreter.node.expression.builtin.number.decimal.PowMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "/",
        org.enso.interpreter.node.expression.builtin.number.decimal.DivideMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "negate",
        org.enso.interpreter.node.expression.builtin.number.decimal.NegateMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "abs",
        org.enso.interpreter.node.expression.builtin.number.decimal.AbsMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "==",
        org.enso.interpreter.node.expression.builtin.number.decimal.EqualsMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        ">",
        org.enso.interpreter.node.expression.builtin.number.decimal.GreaterMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        ">=",
        org.enso.interpreter.node.expression.builtin.number.decimal.GreaterOrEqualMethodGen
            .makeFunction(language));
    scope.registerMethod(
        decimal,
        "<",
        org.enso.interpreter.node.expression.builtin.number.decimal.LessMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "<=",
        org.enso.interpreter.node.expression.builtin.number.decimal.LessOrEqualMethodGen
            .makeFunction(language));
    scope.registerMethod(
        decimal,
        "to_decimal",
        org.enso.interpreter.node.expression.builtin.number.decimal.ToDecimalMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "floor",
        org.enso.interpreter.node.expression.builtin.number.decimal.FloorMethodGen.makeFunction(
            language));
    scope.registerMethod(
        decimal,
        "ceil",
        org.enso.interpreter.node.expression.builtin.number.decimal.CeilMethodGen.makeFunction(
            language));
  }

  /** @return the Int64 atom constructor. */
  public AtomConstructor getSmallInteger() {
    return smallInteger;
  }

  /** @return the Big_Integer atom constructor. */
  public AtomConstructor getBigInteger() {
    return bigInteger;
  }

  /** @return the Integer atom constructor */
  public AtomConstructor getInteger() {
    return integer;
  }

  /** @return the Number atom constructor */
  public AtomConstructor getNumber() {
    return number;
  }

  /** @return the Decimal atom constructor */
  public AtomConstructor getDecimal() {
    return decimal;
  }
}
