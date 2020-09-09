package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container for all number-related builtins. */
public class Number {
  private final AtomConstructor int64;
  private final AtomConstructor bigInteger;
  private final AtomConstructor integer;
  private final AtomConstructor number;

  /**
   * Creates and registers number builtins.
   *
   * @param language the current language instance.
   * @param scope the builtins scope.
   */
  public Number(Language language, ModuleScope scope) {
    number = new AtomConstructor("Number", scope).initializeFields();
    int64 = new AtomConstructor("Int_64", scope).initializeFields();
    integer = new AtomConstructor("Integer", scope).initializeFields();
    bigInteger = new AtomConstructor("Big_Integer", scope).initializeFields();

    registerInt64Methods(language, scope);
    registerBigIntegerMethods(language, scope);

    scope.registerConstructor(number);
    scope.registerConstructor(int64);
    scope.registerConstructor(integer);
    scope.registerConstructor(bigInteger);
  }

  private void registerInt64Methods(Language language, ModuleScope scope) {
    scope.registerMethod(
        int64,
        "+",
        org.enso.interpreter.node.expression.builtin.number.int64.AddMethodGen.makeFunction(
            language));
    scope.registerMethod(
        int64,
        "-",
        org.enso.interpreter.node.expression.builtin.number.int64.SubtractMethodGen.makeFunction(
            language));
    scope.registerMethod(
        int64,
        "*",
        org.enso.interpreter.node.expression.builtin.number.int64.MultiplyMethodGen.makeFunction(
            language));
    scope.registerMethod(
        int64,
        "/",
        org.enso.interpreter.node.expression.builtin.number.int64.DivideMethodGen.makeFunction(
            language));
    scope.registerMethod(
        int64,
        "%",
        org.enso.interpreter.node.expression.builtin.number.int64.ModMethodGen.makeFunction(
            language));
    scope.registerMethod(
        int64,
        "negate",
        org.enso.interpreter.node.expression.builtin.number.int64.NegateMethodGen.makeFunction(
            language));
    scope.registerMethod(
        int64,
        "==",
        org.enso.interpreter.node.expression.builtin.number.int64.EqualsMethodGen.makeFunction(
            language));
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
        "/",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.DivideMethodGen.makeFunction(
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
        "==",
        org.enso.interpreter.node.expression.builtin.number.bigInteger.EqualsMethodGen.makeFunction(
            language));
  }

  /** @return the Int64 atom constructor. */
  public AtomConstructor getInt64() {
    return int64;
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
}
