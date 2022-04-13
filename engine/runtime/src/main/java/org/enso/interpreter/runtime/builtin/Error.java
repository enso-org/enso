package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.error.displaytext.*;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container for builtin Error types */
public class Error {
  private final AtomConstructor syntaxError;
  private final AtomConstructor typeError;
  private final AtomConstructor compileError;
  private final AtomConstructor inexhaustivePatternMatchError;
  private final AtomConstructor uninitializedState;
  private final AtomConstructor noSuchMethodError;
  private final AtomConstructor noSuchConversionError;
  private final AtomConstructor polyglotError;
  private final AtomConstructor moduleNotInPackageError;
  private final AtomConstructor arithmeticError;
  private final AtomConstructor invalidArrayIndexError;
  private final AtomConstructor arityError;
  private final AtomConstructor unsupportedArgumentsError;
  private final AtomConstructor moduleDoesNotExistError;
  private final AtomConstructor notInvokableError;
  private final AtomConstructor invalidConversionTargetError;

  private final Atom arithmeticErrorShiftTooBig;
  private final Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

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
    typeError =
        new AtomConstructor("Type_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "expected", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "actual", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(2, "name", ArgumentDefinition.ExecutionMode.EXECUTE));
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

    noSuchConversionError =
        new AtomConstructor("No_Such_Conversion_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "target", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "that", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(2, "conversion", ArgumentDefinition.ExecutionMode.EXECUTE));

    invalidConversionTargetError =
        new AtomConstructor("Invalid_Conversion_Target_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "target", ArgumentDefinition.ExecutionMode.EXECUTE));

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
    arithmeticErrorDivideByZero = arithmeticError.newInstance(divideByZeroMessage);
    invalidArrayIndexError =
        new AtomConstructor("Invalid_Array_Index_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "array", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "index", ArgumentDefinition.ExecutionMode.EXECUTE));
    arityError =
        new AtomConstructor("Arity_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "expected_min", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "expected_max", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(2, "actual", ArgumentDefinition.ExecutionMode.EXECUTE));

    unsupportedArgumentsError =
        new AtomConstructor("Unsupported_Argument_Types", scope)
            .initializeFields(
                new ArgumentDefinition(0, "arguments", ArgumentDefinition.ExecutionMode.EXECUTE));
    moduleDoesNotExistError =
        new AtomConstructor("Module_Does_Not_Exist", scope)
            .initializeFields(
                new ArgumentDefinition(0, "name", ArgumentDefinition.ExecutionMode.EXECUTE));
    notInvokableError =
        new AtomConstructor("Not_Invokable_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "target", ArgumentDefinition.ExecutionMode.EXECUTE));

    scope.registerConstructor(arityError);
    scope.registerMethod(
        arityError, "to_display_text", ArityErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(syntaxError);
    scope.registerMethod(
        syntaxError, "to_display_text", SyntaxErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(typeError);
    scope.registerMethod(
        typeError, "to_display_text", TypeErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(compileError);
    scope.registerMethod(
        compileError, "to_display_text", CompileErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(inexhaustivePatternMatchError);
    scope.registerMethod(
        inexhaustivePatternMatchError,
        "to_display_text",
        InexhaustivePatternMatchErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(uninitializedState);
    scope.registerMethod(
        uninitializedState,
        "to_display_text",
        UninitializedStateErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(noSuchMethodError);
    scope.registerMethod(
        noSuchMethodError,
        "to_display_text",
        NoSuchMethodErrorToDisplayTextMethodGen.makeFunction(language));

    scope.registerConstructor(noSuchConversionError);
    scope.registerMethod(
        noSuchConversionError,
        "to_display_text",
        NoSuchConversionErrorToDisplayTextMethodGen.makeFunction(language));

    scope.registerConstructor(invalidConversionTargetError);
    scope.registerMethod(
        invalidConversionTargetError,
        "to_display_text",
        InvalidConversionTargetErrorToDisplayTextMethodGen.makeFunction(language));

    scope.registerConstructor(polyglotError);
    scope.registerMethod(
        polyglotError,
        "to_display_text",
        PolyglotErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(moduleNotInPackageError);
    scope.registerMethod(
        moduleNotInPackageError,
        "to_display_text",
        ModuleNotInPackageErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(arithmeticError);
    scope.registerMethod(
        arithmeticError,
        "to_display_text",
        ArithmeticErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(invalidArrayIndexError);
    scope.registerMethod(
        invalidArrayIndexError,
        "to_display_text",
        InvalidArrayIndexErrorToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(unsupportedArgumentsError);
    scope.registerMethod(
        unsupportedArgumentsError,
        "to_display_text",
        UnsupportedArgumentTypesToDisplayTextMethodGen.makeFunction(language));
    scope.registerConstructor(notInvokableError);
    scope.registerMethod(
        notInvokableError,
        "to_display_text",
        NotInvokableErrorToDisplayTextMethodGen.makeFunction(language));
  }

  /** @return the builtin {@code Syntax_Error} atom constructor. */
  public AtomConstructor syntaxError() {
    return syntaxError;
  }

  /** @return the builtin {@code Type_Error} atom constructor. */
  public AtomConstructor typeError() {
    return typeError;
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

  public Atom makeNoSuchConversionError(
      Object target, Object that, UnresolvedConversion conversion) {
    return noSuchConversionError.newInstance(target, that, conversion);
  }

  public Atom makeInvalidConversionTargetError(Object target) {
    return invalidConversionTargetError.newInstance(target);
  }

  /**
   * Creates an instance of the runtime representation of a {@code Type_Error}.
   *
   * @param expected the expected type
   * @param actual the actual type
   * @param name the name of the variable that is a type error
   * @return a runtime representation of the error.
   */
  public Atom makeTypeError(Object expected, Object actual, String name) {
    return typeError.newInstance(expected, actual, Text.create(name));
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

  /** @return An Arithmetic error representing a division by zero. */
  public Atom getDivideByZeroError() {
    return arithmeticErrorDivideByZero;
  }

  /**
   * @param array the array
   * @param index the index
   * @return An error representing that the {@code index} is not valid in {@code array}
   */
  public Atom makeInvalidArrayIndexError(Object array, Object index) {
    return invalidArrayIndexError.newInstance(array, index);
  }

  /**
   * @param expected_min the minimum expected arity
   * @param expected_max the maximum expected arity
   * @param actual the actual arity
   * @return an error informing about the arity being mismatched
   */
  public Atom makeArityError(long expected_min, long expected_max, long actual) {
    return arityError.newInstance(expected_min, expected_max, actual);
  }

  /**
   * @param args an array containing objects
   * @return an error informing about the particular assortment of arguments not being valid for a
   *     given method callp
   */
  public Atom makeUnsupportedArgumentsError(Object[] args) {
    return unsupportedArgumentsError.newInstance(new Array(args));
  }

  /**
   * @param name the name of the module that doesn't exist
   * @return a module does not exist error
   */
  public Atom makeModuleDoesNotExistError(String name) {
    return moduleDoesNotExistError.newInstance(Text.create(name));
  }

  /**
   * @param target the target attempted to be invoked
   * @return a not invokable error
   */
  public Atom makeNotInvokableError(Object target) {
    return notInvokableError.newInstance(target);
  }
}
