package org.enso.interpreter.runtime.builtin;

import static com.oracle.truffle.api.CompilerDirectives.transferToInterpreterAndInvalidate;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.DataflowError;

/** Container for builtin Error types */
public final class Error {
  private final EnsoContext context;
  private final AtomFactory syntaxError;
  private final AtomFactory typeError;
  private final AtomFactory compileError;
  private final AtomFactory assertionError;
  private final AtomFactory indexOutOfBounds;
  private final AtomFactory inexhaustivePatternMatch;
  private final AtomFactory uninitializedState;
  private final AtomFactory noSuchMethod;
  private final AtomFactory noSuchConversion;
  private final AtomFactory noConversionCurrying;
  private final AtomFactory moduleNotInPackageError;
  private final AtomFactory arithmeticError;
  private final AtomFactory invalidArrayIndex;
  private final AtomFactory arityError;
  private final AtomFactory incomparableValues;
  private final AtomFactory unsupportedArgumentsError;
  private final AtomFactory moduleDoesNotExistError;
  private final AtomFactory notInvokable;
  private final AtomFactory noSuchArgument;
  private final AtomFactory missingArgument;
  private final AtomFactory privateAccessError;
  private final AtomFactory invalidConversionTarget;
  private final AtomFactory noSuchField;
  private final AtomFactory numberParseError;
  private final org.enso.interpreter.node.expression.builtin.error.Panic panic;
  private final AtomFactory caughtPanic;
  private final AtomFactory forbiddenOperation;
  private final AtomFactory additionalWarnings;
  private final AtomFactory mapError;
  private final AtomFactory unimplemented;
  private final AtomFactory noWrap;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorShiftTooBig;

  @CompilerDirectives.CompilationFinal private Atom arithmeticErrorDivideByZero;

  private static final Text shiftTooBigMessage = Text.create("Shift amount too large.");
  private static final Text divideByZeroMessage = Text.create("Cannot divide by zero.");

  /** Creates builders for error Atom Constructors. */
  Error(Builtins builtins, EnsoContext context) {
    this.context = context;
    syntaxError = createErrorsCommon("Syntax_Error");
    typeError = createErrorsCommon("Type_Error");
    compileError = createErrorsCommon("Compile_Error");
    assertionError = createErrorsCommon("Assertion_Error");
    indexOutOfBounds = createErrorsCommon("Index_Out_Of_Bounds");
    inexhaustivePatternMatch = createErrorsCommon("Inexhaustive_Pattern_Match");
    uninitializedState = createErrorsCommon("Uninitialized_State");
    noSuchMethod = createErrorsCommon("No_Such_Method");
    noSuchConversion = createErrorsCommon("No_Such_Conversion");
    noConversionCurrying = createErrorsCommon("No_Conversion_Currying");
    moduleNotInPackageError = createErrorsCommon("Module_Not_In_Package_Error");
    arithmeticError = createErrorsCommon("Arithmetic_Error");
    invalidArrayIndex = createErrorsCommon("Invalid_Array_Index");
    arityError = createErrorsCommon("Arity_Error");
    incomparableValues = createErrorsCommon("Incomparable_Values");
    unsupportedArgumentsError = createErrorsCommon("Unsupported_Argument_Types");
    moduleDoesNotExistError = createErrorsCommon("Module_Does_Not_Exist");
    notInvokable = createErrorsCommon("Not_Invokable");
    noSuchArgument = createErrorsCommon("No_Such_Argument");
    missingArgument = createErrorsCommon("Missing_Argument");
    privateAccessError = createErrorsCommon("Private_Access");
    invalidConversionTarget = createErrorsCommon("Invalid_Conversion_Target");
    noSuchField = createErrorsCommon("No_Such_Field");
    panic = builtins.getBuiltinType(org.enso.interpreter.node.expression.builtin.error.Panic.class);
    forbiddenOperation = createErrorsCommon("Forbidden_Operation");
    additionalWarnings = createErrorsCommon("Additional_Warnings");

    numberParseError = new AtomFactory("Data", "Numbers", "Number_Parse_Error");
    caughtPanic = new AtomFactory("Panic", "Caught_Panic");
    unimplemented = new AtomFactory("Errors", "Unimplemented", "Unimplemented");
    mapError = new AtomFactory("Data", "Vector", "Map_Error");
    noWrap = new AtomFactory("Data", "Vector", "No_Wrap");
  }

  public Atom makeSyntaxError(String message) {
    return syntaxError.newInstance(Text.create(message));
  }

  public Atom makeCompileError(String message) {
    return compileError.newInstance(Text.create(message));
  }

  public Atom makeAssertionError(String text) {
    return assertionError.newInstance(Text.create(text));
  }

  public Atom makeIndexOutOfBounds(long index, long length) {
    return indexOutOfBounds.newInstance(index, length);
  }

  public Atom makeIncomparableValues(Object leftOperand, Object rightOperand) {
    return incomparableValues.newInstance(leftOperand, rightOperand);
  }

  public Atom makeInexhaustivePatternMatch(Object message) {
    return inexhaustivePatternMatch.newInstance(message);
  }

  public Atom makeUninitializedStateError(Object key) {
    return uninitializedState.newInstance(key);
  }

  public Atom makeModuleNotInPackageError() {
    return moduleNotInPackageError.newInstance();
  }

  public Type panic() {
    return panic.getType();
  }

  public Type caughtPanic() {
    return caughtPanic.getType();
  }

  public Atom newCaughtPanic(Object payload, AbstractTruffleException originalException) {
    return caughtPanic.newInstance(payload, originalException);
  }

  /**
   * Creates an instance of the runtime representation of a {@code No_Such_Method.Error}.
   *
   * @param target the method call target
   * @param symbol the method being called
   * @return a runtime representation of the error
   */
  public Atom makeNoSuchMethod(Object target, UnresolvedSymbol symbol) {
    return noSuchMethod.newInstance(target, symbol);
  }

  public Atom makeNoSuchFieldError(Atom atom, Text name) {
    return noSuchField.newInstance(atom, name);
  }

  public Atom makeNoSuchConversion(Object target, Object that, UnresolvedConversion conversion) {
    return noSuchConversion.newInstance(target, that, conversion);
  }

  public Atom makeInvalidConversionTarget(Object target) {
    return invalidConversionTarget.newInstance(target);
  }

  public Atom makeNoConversionCurrying(
      boolean hasThis, boolean hasThat, UnresolvedConversion conversion) {
    return noConversionCurrying.newInstance(hasThis, hasThat, conversion);
  }

  /**
   * Creates an instance of the runtime representation of a {@code Type_Error}.
   *
   * @param expected the expected type
   * @param actual the actual type
   * @param name name of the argument that was being checked
   * @return a runtime representation of the error.
   */
  @CompilerDirectives.TruffleBoundary
  public Atom makeTypeError(Object expected, Object actual, String name) {
    return typeError.newInstance(
        expected, actual, Text.create("Expected `" + name + "` to be {exp}, but got {got}"));
  }

  /**
   * Creates an instance of the runtime representation of a {@code Type_Error}.
   *
   * @param expected the expected type
   * @param actual the actual type
   * @param comment description of the value that was being checked
   * @return a runtime representation of the error.
   */
  public Atom makeTypeErrorOfComment(Object expected, Object actual, Text comment) {
    return typeError.newInstance(expected, actual, comment);
  }

  /**
   * Checks whether given atom represents a type error.
   *
   * @param payload the atom to check
   * @return true or false
   */
  public boolean isTypeError(Atom payload) {
    if (payload instanceof Atom atom) {
      return typeError.getUniqueConstructor() == atom.getConstructor();
    } else {
      return false;
    }
  }

  /**
   * Checks whether given atom represents a conversion error.
   *
   * @param payload the atom to check
   * @return true or false
   */
  public boolean isNoSuchConversionError(Object payload) {
    if (payload instanceof Atom atom) {
      return noSuchConversion.getUniqueConstructor() == atom.getConstructor();
    } else {
      return false;
    }
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
  public Atom makeInvalidArrayIndex(Object array, Object index) {
    return invalidArrayIndex.newInstance(array, index);
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
    return unsupportedArgumentsError.newInstance(
        ArrayLikeHelpers.wrapObjectsWithCheckAt(args), Text.create(message));
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
    return notInvokable.newInstance(target, context.getNothing());
  }

  /**
   * @param target the target attempted to be invoked
   * @param cause additional information on what caused the error
   * @return a not invokable error
   */
  public Atom makeNotInvokableWithCause(Object target, Object cause) {
    if (cause == null) {
      cause = context.getNothing();
    }
    return notInvokable.newInstance(target, cause);
  }

  /**
   * Constructs an error that indicates that a named argument application could not find a matching
   * parameter.
   *
   * @param argumentName name of the named argument being applied
   * @return a no such argument error
   */
  public Atom makeNoSuchArgument(String argumentName) {
    return noSuchArgument.newInstance(Text.create(argumentName));
  }

  /**
   * Constructs an error that indicates that a function call was missing an argument for the
   * parameter.
   *
   * @param argumentName name of the missing argument
   * @param functionName name of function missing the argument
   * @return a missing argument error
   */
  public Atom makeMissingArgument(String argumentName, String functionName) {
    return missingArgument.newInstance(
        Text.create(argumentName),
        Text.create(functionName),
        context.getNothing(),
        Text.create("Missing argument for " + argumentName));
  }

  /**
   * @param thisProjectName Current project name. May be null.
   * @param targetProjectName Target method project name. May be null.
   * @param targetMethodName Name of the method that is project-private and cannot be accessed.
   * @param msg special message or {@code null} to construct default message
   */
  public Atom makePrivateAccessError(
      String thisProjectName, String targetProjectName, String targetMethodName, String msg) {
    assert targetMethodName != null;
    EnsoObject thisProjName =
        thisProjectName != null ? Text.create(thisProjectName) : context.getNothing();
    EnsoObject targetProjName =
        targetProjectName != null ? Text.create(targetProjectName) : context.getNothing();
    EnsoObject msgOrNothing = msg != null ? Text.create(msg) : context.getNothing();
    return privateAccessError.newInstance(
        thisProjName, targetProjName, Text.create(targetMethodName), msgOrNothing);
  }

  public Atom makeUnimplemented(String operation) {
    return unimplemented.newInstance(operation);
  }

  public Atom makeNumberParseError(String message) {
    return numberParseError.newInstance(Text.create(message));
  }

  /**
   * @param index the position at which the original error occured
   * @param innerError the original error
   * @return an error indicating the index of the error
   */
  public Atom makeMapError(long index, Object innerError) {
    return mapError.newInstance(index, innerError);
  }

  /**
   * Creates error on missing polyglot java import class.
   *
   * @param className the name of the class that is missing
   * @return data flow error representing the missing value
   */
  public DataflowError makeMissingPolyglotImportError(String className) {
    var msg = "No polyglot symbol for " + className;
    var err = makeCompileError(msg);
    return DataflowError.withDefaultTrace(err, null);
  }

  public Atom makeAdditionalWarnings(long cnt) {
    return additionalWarnings.newInstance(cnt);
  }

  final boolean isNoWrapBuiltin(AtomConstructor cons) {
    return noWrap.getUniqueConstructor() == cons;
  }

  private AtomFactory createErrorsCommon(String typeName) {
    return new AtomFactory("Errors", "Common", typeName);
  }

  private final class AtomFactory {
    private final String[] shortFqn;
    private AtomConstructor uniqueAtomConstructor;

    private AtomFactory(String... shortFqn) {
      this.shortFqn = shortFqn;
    }

    final Atom newInstance(Object... args) {
      return org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode.getUncached()
          .newInstance(getUniqueConstructor(), args);
    }

    final Type getType() {
      return getUniqueConstructor().getType();
    }

    final AtomConstructor getUniqueConstructor() {
      if (uniqueAtomConstructor == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        var sb = new StringBuilder();
        sb.append("Standard.Base");
        var last = shortFqn.length - 1;
        for (var i = 0; i < last; i++) {
          var segment = shortFqn[i];
          sb.append(".").append(segment);
        }
        var moduleOpt = context.getTopScope().getModule(sb.toString());
        if (moduleOpt.isEmpty()) {
          var stdBase = LibraryName.apply("Standard", "Base");
          context.getPackageRepository().ensurePackageIsLoaded(stdBase);
          moduleOpt = context.getTopScope().getModule(sb.toString());
        }
        assert moduleOpt.isPresent() : sb.toString();
        var module = moduleOpt.get();
        var scope = module.compileScope(context);
        var type = scope.getType(shortFqn[last], true);
        assert type != null : shortFqn[last] + " in " + sb;
        assert type.getConstructors().size() == 1
            : "Only one constructor available: " + type.getConstructors();
        uniqueAtomConstructor = type.getConstructors().values().iterator().next();
      }
      return uniqueAtomConstructor;
    }
  }
}
