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

/** Container for builtin Error types */
public class Error {

  private Builtins builtins;
  @CompilerDirectives.CompilationFinal
  private Atom arithmeticErrorShiftTooBig;
  @CompilerDirectives.CompilationFinal
  private Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

  /**
   * Creates and registers the relevant constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors in.
   */
  public Error(Builtins builtins) {
    this.builtins = builtins;
    arithmeticErrorShiftTooBig = null;
    arithmeticErrorDivideByZero = null;
  }

  /** @return the builtin {@code Syntax_Error} atom constructor. */
  public AtomConstructor syntaxError() {
    return builtins.getBuiltinType(SyntaxError.class);
  }

  /** @return the builtin {@code Type_Error} atom constructor. */
  public AtomConstructor typeError() {
    return builtins.getBuiltinType(TypeError.class);
  }

  /** @return the builtin {@code Compile_Error} atom constructor. */
  public AtomConstructor compileError() {
    return builtins.getBuiltinType(CompileError.class);
  }

  /** @return the builtin {@code Inexhaustive_Pattern_Match_Error} atom constructor. */
  public AtomConstructor inexhaustivePatternMatchError() {
    return builtins.getBuiltinType(InexhaustivePatternMatchError.class);
  }

  /** @return the builtin {@code Uninitialized_State} atom constructor. */
  public AtomConstructor uninitializedState() {
    return builtins.getBuiltinType(UninitializedState.class);
  }

  /** @return the builtin {@code Module_Not_In_Package_Error} atom constructor. */
  public AtomConstructor moduleNotInPackageError() {
    return builtins.getBuiltinType(ModuleNotInPackageError.class);
  }

  /**
   * Creates an instance of the runtime representation of a {@code No_Such_Method_Error}.
   *
   * @param target the method call target
   * @param symbol the method being called
   * @return a runtime representation of the error
   */
  public Atom makeNoSuchMethodError(Object target, UnresolvedSymbol symbol) {
    return builtins.getBuiltinType(NoSuchMethodError.class).newInstance(target, symbol);
  }

  public Atom makeNoSuchConversionError(
      Object target, Object that, UnresolvedConversion conversion) {
    return builtins.getBuiltinType(NoSuchConversionError.class).newInstance(target, that, conversion);
  }

  public Atom makeInvalidConversionTargetError(Object target) {
    return builtins.getBuiltinType(InvalidConversionTargetError.class).newInstance(target);
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
    return builtins.getBuiltinType(TypeError.class).newInstance(expected, actual, Text.create(name));
  }

  /**
   * Creates an instance of the runtime representation of a {@code Polyglot_Error}.
   *
   * @param cause the cause of the error.
   * @return a runtime representation of the polyglot error.
   */
  public Atom makePolyglotError(Object cause) {
    return builtins.getBuiltinType(PolyglotError.class).newInstance(cause);
  }

  /**
   * Create an instance of the runtime representation of an {@code Arithmetic_Error}.
   *
   * @param reason the reason that the error is being thrown for
   * @return a runtime representation of the arithmetic error
   */
  public Atom makeArithmeticError(Text reason) {
    return builtins.getBuiltinType(ArithmeticError.class).newInstance(reason);
  }

  /** @return An arithmetic error representing a too-large shift for the bit shift. */
  public Atom getShiftAmountTooLargeError() {
    if (arithmeticErrorShiftTooBig == null) {
      arithmeticErrorShiftTooBig = makeArithmeticError(shiftTooBigMessage);
    }
    return arithmeticErrorShiftTooBig;
  }

  /** @return An Arithmetic error representing a division by zero. */
  public Atom getDivideByZeroError() {
    if (arithmeticErrorDivideByZero == null) {
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
    return builtins.getBuiltinType(InvalidArrayIndexError.class).newInstance(array, index);
  }

  /**
   * @param expected_min the minimum expected arity
   * @param expected_max the maximum expected arity
   * @param actual the actual arity
   * @return an error informing about the arity being mismatched
   */
  public Atom makeArityError(long expected_min, long expected_max, long actual) {
    return builtins.getBuiltinType(ArityError.class).newInstance(expected_min, expected_max, actual);
  }

  /**
   * @param args an array containing objects
   * @return an error informing about the particular assortment of arguments not being valid for a
   *     given method callp
   */
  public Atom makeUnsupportedArgumentsError(Object[] args) {
    return builtins.getBuiltinType(UnsupportedArgumentTypes.class).newInstance(new Array(args));
  }

  /**
   * @param name the name of the module that doesn't exist
   * @return a module does not exist error
   */
  public Atom makeModuleDoesNotExistError(String name) {
    return builtins.getBuiltinType(ModuleDoesNotExist.class).newInstance(Text.create(name));
  }

  /**
   * @param target the target attempted to be invoked
   * @return a not invokable error
   */
  public Atom makeNotInvokableError(Object target) {
    return builtins.getBuiltinType(NotInvokableError.class).newInstance(target);
  }
}
