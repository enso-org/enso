package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin Error types */
public class Error {
  private final AtomConstructor syntaxError;
  private final AtomConstructor compileError;
  private final AtomConstructor inexhaustivePatternMatchError;
  private final AtomConstructor uninitializedState;
  private final AtomConstructor noSuchMethodError;
  private final AtomConstructor polyglotError;
  private final AtomConstructor moduleNotInPackageError;
  private final AtomConstructor arithmeticError;

  private final Atom arithmeticErrorShiftTooBig;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");

  /**
   * Creates and registers the relevant constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors in.
   */
  public Error(Language language, ModuleScope scope) {
    syntaxError =
        new AtomConstructor("Syntax_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    compileError =
        new AtomConstructor("Compile_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    inexhaustivePatternMatchError =
        new AtomConstructor("Inexhaustive_Pattern_Match_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "scrutinee", ArgumentDefinition.ExecutionMode.EXECUTE));
    uninitializedState =
        new AtomConstructor("Uninitialized_State", scope)
            .initializeFields(
                new ArgumentDefinition(0, "key", ArgumentDefinition.ExecutionMode.EXECUTE));
    noSuchMethodError =
        new AtomConstructor("No_Such_Method_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "target", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "symbol", ArgumentDefinition.ExecutionMode.EXECUTE));
    polyglotError =
        new AtomConstructor("Polyglot_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "cause", ArgumentDefinition.ExecutionMode.EXECUTE));
    moduleNotInPackageError =
        new AtomConstructor("Module_Not_In_Package_Error", scope).initializeFields();
    arithmeticError =
        new AtomConstructor("Arithmetic_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    arithmeticErrorShiftTooBig = arithmeticError.newInstance(shiftTooBigMessage);

    scope.registerConstructor(syntaxError);
    scope.registerConstructor(compileError);
    scope.registerConstructor(inexhaustivePatternMatchError);
    scope.registerConstructor(uninitializedState);
    scope.registerConstructor(noSuchMethodError);
    scope.registerConstructor(polyglotError);
    scope.registerConstructor(moduleNotInPackageError);
    scope.registerConstructor(arithmeticError);
  }

  /** @return the builtin {@code Syntax_Error} atom constructor. */
  public AtomConstructor syntaxError() {
    return syntaxError;
  }

  /** @return the builtin {@code Compile_Error} atom constructor. */
  public AtomConstructor compileError() {
    return compileError;
  }

  /** @return the builtin {@code Inexhaustive_Pattern_Match_Error} atom constructor. */
  public AtomConstructor inexhaustivePatternMatchError() {
    return inexhaustivePatternMatchError;
  }

  /** @return the builtin {@code Uninitialized_State} atom constructor. */
  public AtomConstructor uninitializedState() {
    return uninitializedState;
  }

  /** @return the builtin {@code Module_Not_In_Package_Error} atom constructor. */
  public AtomConstructor moduleNotInPackageError() {
    return moduleNotInPackageError;
  }

  /**
   * Creates an instance of the runtime representation of a {@code No_Such_Method_Error}.
   *
   * @param target the method call target
   * @param symbol the method being called
   * @return a runtime representation of the error
   */
  public Atom makeNoSuchMethodError(Object target, UnresolvedSymbol symbol) {
    return noSuchMethodError.newInstance(target, symbol);
  }

  /**
   * Creates an instance of the runtime representation of a {@code Polyglot_Error}.
   *
   * @param cause the cause of the error.
   * @return a runtime representation of the polyglot error.
   */
  public Atom makePolyglotError(Object cause) {
    return polyglotError.newInstance(cause);
  }

  /**
   * Create an instance of the runtime representation of an {@code Arithmetic_Error}.
   *
   * @param reason the reason that the error is being thrown for
   * @return a runtime representation of the arithmetic error
   */
  public Atom makeArithmeticError(Text reason) {
    return arithmeticError.newInstance(reason);
  }

  /** @return An arithmetic error representing a too-large shift for the bit shift. */
  public Atom getShiftAmountTooLargeError() {
    return arithmeticErrorShiftTooBig;
  }
}
