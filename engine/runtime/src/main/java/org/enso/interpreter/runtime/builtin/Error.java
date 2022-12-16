package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.error.*;
import org.enso.interpreter.node.expression.builtin.error.NoSuchFieldError;
import org.enso.interpreter.node.expression.builtin.error.NoSuchMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

import static com.oracle.truffle.api.CompilerDirectives.transferToInterpreterAndInvalidate;

/** Container for builtin Error types */
public class Error {
  private final EnsoContext context;
  private final SyntaxError syntaxError;
  private final TypeError typeError;
  private final CompileError compileError;
  private final IndexOutOfBounds indexOutOfBounds;
  private final InexhaustivePatternMatch inexhaustivePatternMatch;
  private final UninitializedState uninitializedState;
  private final NoSuchMethod noSuchMethod;
  private final NoSuchConversion noSuchConversion;
  private final PolyglotError polyglotError;
  private final ModuleNotInPackageError moduleNotInPackageError;
  private final ArithmeticError arithmeticError;
  private final InvalidArrayIndex invalidArrayIndex;
  private final ArityError arityError;
  private final UnsupportedArgumentTypes unsupportedArgumentsError;
  private final ModuleDoesNotExist moduleDoesNotExistError;
  private final NotInvokable notInvokable;
  private final InvalidConversionTarget invalidConversionTarget;
  private final NoSuchFieldError noSuchFieldError;
  private final NumberParseError numberParseError;
  private final Panic panic;
  private final CaughtPanic caughtPanic;
  private final ForbiddenOperation forbiddenOperation;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorShiftTooBig;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

  /** Creates builders for error Atom Constructors. */
  public Error(Builtins builtins, EnsoContext context) {
    this.context = context;
    syntaxError = builtins.getBuiltinType(SyntaxError.class);
    typeError = builtins.getBuiltinType(TypeError.class);
    compileError = builtins.getBuiltinType(CompileError.class);
    indexOutOfBounds = builtins.getBuiltinType(IndexOutOfBounds.class);
    inexhaustivePatternMatch = builtins.getBuiltinType(InexhaustivePatternMatch.class);
    uninitializedState = builtins.getBuiltinType(UninitializedState.class);
    noSuchMethod = builtins.getBuiltinType(NoSuchMethod.class);
    noSuchConversion = builtins.getBuiltinType(NoSuchConversion.class);
    polyglotError = builtins.getBuiltinType(PolyglotError.class);
    moduleNotInPackageError = builtins.getBuiltinType(ModuleNotInPackageError.class);
    arithmeticError = builtins.getBuiltinType(ArithmeticError.class);
    invalidArrayIndex = builtins.getBuiltinType(InvalidArrayIndex.class);
    arityError = builtins.getBuiltinType(ArityError.class);
    unsupportedArgumentsError = builtins.getBuiltinType(UnsupportedArgumentTypes.class);
    moduleDoesNotExistError = builtins.getBuiltinType(ModuleDoesNotExist.class);
    notInvokable = builtins.getBuiltinType(NotInvokable.class);
    invalidConversionTarget = builtins.getBuiltinType(InvalidConversionTarget.class);
    noSuchFieldError = builtins.getBuiltinType(NoSuchFieldError.class);
    numberParseError = builtins.getBuiltinType(NumberParseError.class);
    panic = builtins.getBuiltinType(Panic.class);
    caughtPanic = builtins.getBuiltinType(CaughtPanic.class);
    forbiddenOperation = builtins.getBuiltinType(ForbiddenOperation.class);
  }

  public Atom makeSyntaxError(Object message) {
    return syntaxError.newInstance(message);
  }

  public Atom makeCompileError(Object message) {
    return compileError.newInstance(message);
  }

  public Atom makeIndexOutOfBounds(long index, long length) {
    return indexOutOfBounds.newInstance(index, length);
  }

  public Atom makeInexhaustivePatternMatch(Object message) {
    return inexhaustivePatternMatch.newInstance(message);
  }

  public Atom makeUninitializedStateError(Object key) {
    return uninitializedState.newInstance(key);
  }

  public Type makeModuleNotInPackageError() {
    return moduleNotInPackageError.getType();
  }

  public Type panic() {
    return panic.getType();
  }

  public CaughtPanic caughtPanic() {
    return caughtPanic;
  }

  /**
   * Creates an instance of the runtime representation of a {@code No_Such_Method_Error}.
   *
   * @param target the method call target
   * @param symbol the method being called
   * @return a runtime representation of the error
   */
  public Atom makeNoSuchMethod(Object target, UnresolvedSymbol symbol) {
    return noSuchMethod.newInstance(target, symbol);
  }

  public NoSuchFieldError getNoSuchFieldError() {
    return noSuchFieldError;
  }

  public Atom makeNoSuchConversion(Object target, Object that, UnresolvedConversion conversion) {
    return noSuchConversion.newInstance(target, that, conversion);
  }

  public Atom makeInvalidConversionTarget(Object target) {
    return invalidConversionTarget.newInstance(target);
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

  public PolyglotError getPolyglotError() {
    return polyglotError;
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
  public Atom makeInvalidArrayIndex(Object array, Object index) {
    return invalidArrayIndex.newInstance(array, index);
  }

  public InvalidArrayIndex getInvalidArrayIndex() {
    return invalidArrayIndex;
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
   * @param message A detailed message, or null
   * @return an error informing about the particular assortment of arguments not being valid for a
   *     given method call
   */
  public Atom makeUnsupportedArgumentsError(Object[] args, String message) {
    return unsupportedArgumentsError.newInstance(new Array(args), message);
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
  public Atom makeNotInvokable(Object target) {
    return notInvokable.newInstance(target);
  }

  public ForbiddenOperation getForbiddenOperation() {
    return forbiddenOperation;
  }

  public Atom makeNumberParseError(String message) {
    return numberParseError.newInstance(Text.create(message));
  }
}
