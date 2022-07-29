package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.node.expression.builtin.error.*;
import org.enso.interpreter.node.expression.builtin.error.NoSuchMethodError;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

import static com.oracle.truffle.api.CompilerDirectives.transferToInterpreterAndInvalidate;

/** Container for builtin Error types */
public class Error {

  private final Builtin syntaxError;
  private final Builtin typeError;
  private final Builtin compileError;
  private final Builtin inexhaustivePatternMatchError;
  private final Builtin uninitializedState;
  private final Builtin noSuchMethodError;
  private final Builtin noSuchConversionError;
  private final Builtin polyglotError;
  private final Builtin moduleNotInPackageError;
  private final Builtin arithmeticError;
  private final Builtin invalidArrayIndexError;
  private final Builtin arityError;
  private final Builtin unsupportedArgumentsError;
  private final Builtin moduleDoesNotExistError;
  private final Builtin notInvokableError;
  private final Builtin invalidConversionTargetError;
  private final Builtin panic;
  private final Builtin caughtPanic;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorShiftTooBig;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

  /** Creates builders for error Atom Constructors. */
  public Error(Builtins builtins) {
    syntaxError = builtins.getBuiltinType(SyntaxError.class);
    typeError = builtins.getBuiltinType(TypeError.class);
    compileError = builtins.getBuiltinType(CompileError.class);
    inexhaustivePatternMatchError = builtins.getBuiltinType(InexhaustivePatternMatchError.class);
    uninitializedState = builtins.getBuiltinType(UninitializedState.class);
    noSuchMethodError = builtins.getBuiltinType(NoSuchMethodError.class);
    noSuchConversionError = builtins.getBuiltinType(NoSuchConversionError.class);
    polyglotError = builtins.getBuiltinType(PolyglotError.class);
    moduleNotInPackageError = builtins.getBuiltinType(ModuleNotInPackageError.class);
    arithmeticError = builtins.getBuiltinType(ArithmeticError.class);
    invalidArrayIndexError = builtins.getBuiltinType(InvalidArrayIndexError.class);
    arityError = builtins.getBuiltinType(ArityError.class);
    unsupportedArgumentsError = builtins.getBuiltinType(UnsupportedArgumentTypes.class);
    moduleDoesNotExistError = builtins.getBuiltinType(ModuleDoesNotExist.class);
    notInvokableError = builtins.getBuiltinType(NotInvokableError.class);
    invalidConversionTargetError = builtins.getBuiltinType(InvalidConversionTargetError.class);
    panic = builtins.getBuiltinType(Panic.class);
    caughtPanic = builtins.getBuiltinType(CaughtPanic.class);
  }

  public Atom makeSyntaxError(Object message) {
    return syntaxError.getUniqueConstructor().newInstance(message);
  }

  public Atom makeCompileError(Object message) {
    return compileError.getUniqueConstructor().newInstance(message);
  }

  public Atom makeInexhaustivePatternMatchError(Object message) {
    return inexhaustivePatternMatchError.getUniqueConstructor().newInstance(message);
  }

  public Atom makeUninitializedStateError(Object key) {
    return uninitializedState.getUniqueConstructor().newInstance(key);
  }

  public Atom makeModuleNotInPackageError() {
    return moduleNotInPackageError.getUniqueConstructor().newInstance();
  }

  public Type panic() {
    return panic.getType();
  }

  public Builtin caughtPanic() {
    return caughtPanic;
  }

  /**
   * Creates an instance of the runtime representation of a {@code No_Such_Method_Error}.
   *
   * @param target the method call target
   * @param symbol the method being called
   * @return a runtime representation of the error
   */
  public Atom makeNoSuchMethodError(Object target, UnresolvedSymbol symbol) {
    return noSuchMethodError.getUniqueConstructor().newInstance(target, symbol);
  }

  public Atom makeNoSuchConversionError(
      Object target, Object that, UnresolvedConversion conversion) {
    return noSuchConversionError.getUniqueConstructor().newInstance(target, that, conversion);
  }

  public Atom makeInvalidConversionTargetError(Object target) {
    return invalidConversionTargetError.getUniqueConstructor().newInstance(target);
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
    return typeError.getUniqueConstructor().newInstance(expected, actual, Text.create(name));
  }

  /**
   * Creates an instance of the runtime representation of a {@code Polyglot_Error}.
   *
   * @param cause the cause of the error.
   * @return a runtime representation of the polyglot error.
   */
  public Atom makePolyglotError(Object cause) {
    return polyglotError.getUniqueConstructor().newInstance(cause);
  }

  /**
   * Create an instance of the runtime representation of an {@code Arithmetic_Error}.
   *
   * @param reason the reason that the error is being thrown for
   * @return a runtime representation of the arithmetic error
   */
  private Atom makeArithmeticError(Text reason) {
    return arithmeticError.getUniqueConstructor().newInstance(reason);
  }

  /**
   * @return An arithmetic error representing a too-large shift for the bit shift.
   */
  public Atom getShiftAmountTooLargeError() {
    if (arithmeticErrorShiftTooBig == null) {
      transferToInterpreterAndInvalidate();
      arithmeticErrorShiftTooBig = makeArithmeticError(shiftTooBigMessage);
    }
    return arithmeticErrorShiftTooBig;
  }

  /**
   * @return An Arithmetic error representing a division by zero.
   */
  public Atom getDivideByZeroError() {
    if (arithmeticErrorDivideByZero == null) {
      transferToInterpreterAndInvalidate();
      arithmeticErrorDivideByZero = makeArithmeticError(divideByZeroMessage);
    }
    return arithmeticErrorDivideByZero;
  }

  /**
   * @param array the array
   * @param index the index
   * @return An error representing that the {@code index} is not valid in {@code array}
   */
  public Atom makeInvalidArrayIndexError(Object array, Object index) {
    return invalidArrayIndexError.getUniqueConstructor().newInstance(array, index);
  }

  /**
   * @param expected_min the minimum expected arity
   * @param expected_max the maximum expected arity
   * @param actual the actual arity
   * @return an error informing about the arity being mismatched
   */
  public Atom makeArityError(long expected_min, long expected_max, long actual) {
    return arityError.getUniqueConstructor().newInstance(expected_min, expected_max, actual);
  }

  /**
   * @param args an array containing objects
   * @return an error informing about the particular assortment of arguments not being valid for a
   *     given method callp
   */
  public Atom makeUnsupportedArgumentsError(Object[] args) {
    return unsupportedArgumentsError.getUniqueConstructor().newInstance(new Array(args));
  }

  /**
   * @param name the name of the module that doesn't exist
   * @return a module does not exist error
   */
  public Atom makeModuleDoesNotExistError(String name) {
    return moduleDoesNotExistError.getUniqueConstructor().newInstance(Text.create(name));
  }

  /**
   * @param target the target attempted to be invoked
   * @return a not invokable error
   */
  public Atom makeNotInvokableError(Object target) {
    return notInvokableError.getUniqueConstructor().newInstance(target);
  }
}
