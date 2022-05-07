package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.error.*;
import org.enso.interpreter.node.expression.builtin.error.NoSuchMethodError;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;

import static com.oracle.truffle.api.CompilerDirectives.transferToInterpreterAndInvalidate;

/** Container for builtin Error types */
public class Error {

  private final BuiltinAtomConstructor syntaxError;
  private final BuiltinAtomConstructor typeError;
  private final BuiltinAtomConstructor compileError;
  private final BuiltinAtomConstructor inexhaustivePatternMatchError;
  private final BuiltinAtomConstructor uninitializedState;
  private final BuiltinAtomConstructor noSuchMethodError;
  private final BuiltinAtomConstructor noSuchConversionError;
  private final BuiltinAtomConstructor polyglotError;
  private final BuiltinAtomConstructor moduleNotInPackageError;
  private final BuiltinAtomConstructor arithmeticError;
  private final BuiltinAtomConstructor invalidArrayIndexError;
  private final BuiltinAtomConstructor arityError;
  private final BuiltinAtomConstructor unsupportedArgumentsError;
  private final BuiltinAtomConstructor moduleDoesNotExistError;
  private final BuiltinAtomConstructor notInvokableError;
  private final BuiltinAtomConstructor invalidConversionTargetError;
  private final BuiltinAtomConstructor panic;
  private final BuiltinAtomConstructor caughtPanic;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorShiftTooBig;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

  /** Creates builders for error Atom Constructors. */
  public Error(Builtins builtins) {
    syntaxError = new BuiltinAtomConstructor(builtins, SyntaxError.class);
    typeError = new BuiltinAtomConstructor(builtins, TypeError.class);
    compileError = new BuiltinAtomConstructor(builtins, CompileError.class);
    inexhaustivePatternMatchError =
        new BuiltinAtomConstructor(builtins, InexhaustivePatternMatchError.class);
    uninitializedState = new BuiltinAtomConstructor(builtins, UninitializedState.class);
    noSuchMethodError = new BuiltinAtomConstructor(builtins, NoSuchMethodError.class);
    noSuchConversionError = new BuiltinAtomConstructor(builtins, NoSuchConversionError.class);
    polyglotError = new BuiltinAtomConstructor(builtins, PolyglotError.class);
    moduleNotInPackageError = new BuiltinAtomConstructor(builtins, ModuleNotInPackageError.class);
    arithmeticError = new BuiltinAtomConstructor(builtins, ArithmeticError.class);
    invalidArrayIndexError = new BuiltinAtomConstructor(builtins, InvalidArrayIndexError.class);
    arityError = new BuiltinAtomConstructor(builtins, ArityError.class);
    unsupportedArgumentsError =
        new BuiltinAtomConstructor(builtins, UnsupportedArgumentTypes.class);
    moduleDoesNotExistError = new BuiltinAtomConstructor(builtins, ModuleDoesNotExist.class);
    notInvokableError = new BuiltinAtomConstructor(builtins, NotInvokableError.class);
    invalidConversionTargetError =
        new BuiltinAtomConstructor(builtins, InvalidConversionTargetError.class);
    panic = new BuiltinAtomConstructor(builtins, Panic.class);
    caughtPanic = new BuiltinAtomConstructor(builtins, CaughtPanic.class);
  }

  public Atom makeSyntaxError(Object message) {
    return syntaxError.newInstance(message);
  }

  public Atom makeCompileError(Object message) {
    return compileError.newInstance(message);
  }

  public Atom makeInexhaustivePatternMatchError(Object message) {
    return inexhaustivePatternMatchError.newInstance(message);
  }

  public Atom makeUninitializedStateError(Object key) {
    return uninitializedState.newInstance(key);
  }

  public Atom makeModuleNotInPackageError() {
    return moduleNotInPackageError.newInstance();
  }

  public AtomConstructor panic() {
    return panic.constructor();
  }

  public AtomConstructor caughtPanic() {
    return caughtPanic.constructor();
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
  private Atom makeArithmeticError(Text reason) {
    return arithmeticError.newInstance(reason);
  }

  /** @return An arithmetic error representing a too-large shift for the bit shift. */
  public Atom getShiftAmountTooLargeError() {
    if (arithmeticErrorShiftTooBig == null) {
      transferToInterpreterAndInvalidate();
      arithmeticErrorShiftTooBig = makeArithmeticError(shiftTooBigMessage);
    }
    return arithmeticErrorShiftTooBig;
  }

  /** @return An Arithmetic error representing a division by zero. */
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
