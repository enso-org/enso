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

  @CompilerDirectives.CompilationFinal
  private AtomConstructor syntaxError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor typeError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor compileError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor inexhaustivePatternMatchError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor uninitializedState;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor noSuchMethodError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor noSuchConversionError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor polyglotError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor moduleNotInPackageError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor arithmeticError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor invalidArrayIndexError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor arityError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor unsupportedArgumentsError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor moduleDoesNotExistError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor notInvokableError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor invalidConversionTargetError;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor panic;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor caughtPanic;

  @CompilerDirectives.CompilationFinal
  private Atom arithmeticErrorShiftTooBig;

  @CompilerDirectives.CompilationFinal
  private Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

  private final Builtins builtins;

  public Error(Builtins builtins) {
    this.builtins = builtins;
  }

  public Atom makeSyntaxError(Object message) {
    if (syntaxError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      syntaxError = builtins.getBuiltinType(SyntaxError.class);
    }
    return syntaxError.newInstance(message);
  }

  public Atom makeCompileError(Object message) {
    if (compileError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      compileError = builtins.getBuiltinType(CompileError.class);
    }
    return compileError.newInstance(message);
  }

  public Atom makeInexhaustivePatternMatchError(Object message) {
    if (inexhaustivePatternMatchError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      inexhaustivePatternMatchError = builtins.getBuiltinType(InexhaustivePatternMatchError.class);
    }
    return inexhaustivePatternMatchError.newInstance(message);
  }

  public Atom makeUninitializedStateError(Object key) {
    if (uninitializedState == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      uninitializedState = builtins.getBuiltinType(UninitializedState.class);
    }
    return uninitializedState.newInstance(key);
  }

  public Atom makeModuleNotInPackageError() {
    if (moduleNotInPackageError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      moduleNotInPackageError = builtins.getBuiltinType(ModuleNotInPackageError.class);
    }
    return moduleNotInPackageError.newInstance();
  }

  public AtomConstructor panic() {
    if (panic == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      panic = builtins.getBuiltinType(Panic.class);
    }
    return panic;
  }

  public AtomConstructor caughtPanic() {
    if (caughtPanic == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      caughtPanic = builtins.getBuiltinType(CaughtPanic.class);
    }
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
    if (noSuchMethodError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      noSuchMethodError = builtins.getBuiltinType(NoSuchMethodError.class);
    }
    return noSuchMethodError.newInstance(target, symbol);
  }

  public Atom makeNoSuchConversionError(
      Object target, Object that, UnresolvedConversion conversion) {
    if (noSuchConversionError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      noSuchConversionError = builtins.getBuiltinType(NoSuchConversionError.class);
    }
    return noSuchConversionError.newInstance(target, that, conversion);
  }

  public Atom makeInvalidConversionTargetError(Object target) {
    if (invalidConversionTargetError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      invalidConversionTargetError = builtins.getBuiltinType(InvalidConversionTargetError.class);
    }
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
    if (typeError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      typeError = builtins.getBuiltinType(TypeError.class);
    }
    return typeError.newInstance(expected, actual, Text.create(name));
  }

  /**
   * Creates an instance of the runtime representation of a {@code Polyglot_Error}.
   *
   * @param cause the cause of the error.
   * @return a runtime representation of the polyglot error.
   */
  public Atom makePolyglotError(Object cause) {
    if (polyglotError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      polyglotError = builtins.getBuiltinType(PolyglotError.class);
    }
    return polyglotError.newInstance(cause);
  }

  /**
   * Create an instance of the runtime representation of an {@code Arithmetic_Error}.
   *
   * @param reason the reason that the error is being thrown for
   * @return a runtime representation of the arithmetic error
   */
  public Atom makeArithmeticError(Text reason) {
    if (arithmeticError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      arithmeticError = builtins.getBuiltinType(ArithmeticError.class);
    }
    return arithmeticError.newInstance(reason);
  }

  /** @return An arithmetic error representing a too-large shift for the bit shift. */
  public Atom getShiftAmountTooLargeError() {
    if (arithmeticErrorShiftTooBig == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      arithmeticErrorShiftTooBig = makeArithmeticError(shiftTooBigMessage);
    }
    return arithmeticErrorShiftTooBig;
  }

  /** @return An Arithmetic error representing a division by zero. */
  public Atom getDivideByZeroError() {
    if (arithmeticErrorDivideByZero == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
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
    if (invalidArrayIndexError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      invalidArrayIndexError = builtins.getBuiltinType(InvalidArrayIndexError.class);
    }
    return invalidArrayIndexError.newInstance(array, index);
  }

  /**
   * @param expected_min the minimum expected arity
   * @param expected_max the maximum expected arity
   * @param actual the actual arity
   * @return an error informing about the arity being mismatched
   */
  public Atom makeArityError(long expected_min, long expected_max, long actual) {
    if (arityError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      arityError = builtins.getBuiltinType(ArityError.class);
    }
    return arityError.newInstance(expected_min, expected_max, actual);
  }

  /**
   * @param args an array containing objects
   * @return an error informing about the particular assortment of arguments not being valid for a
   *     given method callp
   */
  public Atom makeUnsupportedArgumentsError(Object[] args) {
    if (unsupportedArgumentsError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      unsupportedArgumentsError = builtins.getBuiltinType(UnsupportedArgumentTypes.class);
    }
    return unsupportedArgumentsError.newInstance(new Array(args));
  }

  /**
   * @param name the name of the module that doesn't exist
   * @return a module does not exist error
   */
  public Atom makeModuleDoesNotExistError(String name) {
    if (moduleDoesNotExistError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      moduleDoesNotExistError = builtins.getBuiltinType(ModuleDoesNotExist.class);
    }
    return moduleDoesNotExistError.newInstance(Text.create(name));
  }

  /**
   * @param target the target attempted to be invoked
   * @return a not invokable error
   */
  public Atom makeNotInvokableError(Object target) {
    if (notInvokableError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      notInvokableError = builtins.getBuiltinType(NotInvokableError.class);
    }
    return notInvokableError.newInstance(target);
  }
}
