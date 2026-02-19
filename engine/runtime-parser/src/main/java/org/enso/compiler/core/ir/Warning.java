package org.enso.compiler.core.ir;

import java.util.List;
import java.util.stream.Collectors;
import org.enso.compiler.core.IR;
import scala.Function1;

/** A trait for all warnings in Enso's IR. */
public sealed interface Warning extends Diagnostic
    permits Warning.DiscardedValue,
        Warning.DuplicatedImport,
        Warning.FailedParallelism,
        Warning.NoSuchMethod,
        Warning.NonUnitTypeUsedOnValueLevel,
        Warning.NotInvokable,
        Warning.Syntax,
        Warning.TypeMismatch,
        Warning.UnusedImport,
        Warning.UnusedSymbolsFromImport,
        Warning.WrongBuiltinMethod,
        Warning.WrongSelfParameterPos,
        Warning.WrongTco,
        org.enso.compiler.core.ir.expression.warnings.Shadowed,
        org.enso.compiler.core.ir.expression.warnings.Unreachable,
        org.enso.compiler.core.ir.expression.warnings.Unused {

  @Override
  default Object[] diagnosticKeys() {
    return new Object[0];
  }

  /**
   * A warning about a duplicated import.
   *
   * @param identifiedLocation the location of the duplicated import
   * @param originalImport the original import that was duplicated
   * @param symbolName the name of the symbol being imported
   */
  record DuplicatedImport(
      IdentifiedLocation identifiedLocation,
      org.enso.compiler.core.ir.module.scope.Import originalImport,
      String symbolName)
      implements Warning {

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      String originalImportRepr;
      if (originalImport.location().isDefined()) {
        originalImportRepr =
            "'"
                + originalImport.showCode()
                + "' in "
                + source.apply(originalImport.location().get());
      } else {
        originalImportRepr = originalImport.showCode();
      }
      return "Duplicated import of "
          + symbolName
          + ". The original import is "
          + originalImportRepr
          + ".";
    }
  }

  /**
   * Warning about unused symbols from an import. Only relevant for imports of form {@code from M
   * import A,B,C}.
   */
  record UnusedSymbolsFromImport(IdentifiedLocation identifiedLocation, List<String> unusedSymbols)
      implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      var unusedSymbolsRepr = unusedSymbols.stream().sorted().collect(Collectors.joining(", "));
      return "Following symbols are not used in this import: [" + unusedSymbolsRepr + "].";
    }
  }

  /**
   * A warning about an unused import.
   *
   * @param identifiedLocation the location of the unused import
   */
  record UnusedImport(IdentifiedLocation identifiedLocation) implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "The import is not used";
    }
  }

  /**
   * A warning about a {@code @Tail_Call} annotation placed in a non-tail position.
   *
   * @param identifiedLocation the location of the annotated application
   */
  record WrongTco(IdentifiedLocation identifiedLocation) implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "A @Tail_Call annotation was placed in a non-tail-call position.";
    }
  }

  /**
   * A warning about an invocation of a value that is not a function.
   *
   * <p>This warning indicates a place that will result in a Not_Invokable error in runtime.
   *
   * @param identifiedLocation the location of the call
   * @param typeRepresentation the type of the value that was called
   */
  record NotInvokable(IdentifiedLocation identifiedLocation, String typeRepresentation)
      implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Invoking a value that has a non-function type "
          + typeRepresentation
          + " will result in a Not_Invokable error in runtime.";
    }
  }

  /**
   * A warning indicating a mismatch between a type expected by an expression and the type that is
   * provided.
   *
   * <p>Currently, this warning is only raised if the mismatch is guaranteed to happen - i.e.
   * running the expression will always result in a runtime Type_Error.
   *
   * @param identifiedLocation the location of the type assertion
   * @param expectedType the type that was expected in the assertion
   * @param actualType the type that was provided
   */
  record TypeMismatch(IdentifiedLocation identifiedLocation, String expectedType, String actualType)
      implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Got an expression of type "
          + actualType
          + " that will never match "
          + expectedType
          + ". This will always result in a Type_Error in runtime.";
    }
  }

  /**
   * A warning about calling a method (or field getter) that is not defined on the given type.
   *
   * <p>This warning indicates a place that will result in a No_Such_Method error in runtime.
   *
   * @param identifiedLocation the location of the call
   * @param methodDescription the description of the method
   */
  record NoSuchMethod(IdentifiedLocation identifiedLocation, String methodDescription)
      implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Calling " + methodDescription + " will result in a No_Such_Method error in runtime.";
    }
  }

  /**
   * A warning about a discarded value.
   *
   * @param identifiedLocation the location of the discarded value
   * @param discardedType the type of the discarded value
   */
  record DiscardedValue(IdentifiedLocation identifiedLocation, String discardedType)
      implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "A value of type " + discardedType + " is discarded.";
    }
  }

  /**
   * A warning about a {@code @Builtin_Method} annotation placed in a method with unexpected body.
   *
   * @param identifiedLocation the location of the annotated application
   */
  record WrongBuiltinMethod(IdentifiedLocation identifiedLocation) implements Warning {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "A @Builtin_Method annotation allows only the name of the builtin node in the body.";
    }
  }

  /**
   * A warning raised when a method is defined with a {@code self} parameter defined not in the
   * first position in the parameters' list.
   *
   * @param funName the function name
   * @param ir the annotated application
   * @param paramPosition the position of the self parameter
   */
  record WrongSelfParameterPos(Name funName, IR ir, int paramPosition) implements Warning {
    @Override
    public IdentifiedLocation identifiedLocation() {
      return ir.identifiedLocation();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return funName.name()
          + ": Self parameter should be declared as the first parameter. "
          + "Instead its position is: "
          + (paramPosition + 1)
          + ".";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {ir.showCode(), paramPosition};
    }
  }

  /**
   * A warning raised when a call is annotated with {@code @Auto_Parallel}, but the annotation
   * cannot be obeyed.
   *
   * @param ir the annotated application
   * @param reason the reason why the annotation cannot be obeyed
   */
  record FailedParallelism(IR ir, String reason) implements Warning {
    @Override
    public IdentifiedLocation identifiedLocation() {
      return ir.identifiedLocation();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "The expression " + ir.showCode() + " could not be parallelised: " + reason + ".";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {ir.showCode(), reason};
    }
  }

  /**
   * A warning raised when a non-unit type is used on value level.
   *
   * @param ir the name IR node
   * @param context the context in which the type is used
   */
  record NonUnitTypeUsedOnValueLevel(Name ir, String context) implements Warning {
    @Override
    public IdentifiedLocation identifiedLocation() {
      return ir.identifiedLocation();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "A non-unit type "
          + ir.name()
          + " is used on value level (in "
          + context
          + ")."
          + " This is probably an error.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {ir.name()};
    }
  }

  /**
   * A warning related to syntax.
   *
   * @param ir the IR node
   * @param syntaxMessage the syntax warning message
   */
  record Syntax(IR ir, String syntaxMessage) implements Warning {
    @Override
    public IdentifiedLocation identifiedLocation() {
      return ir.identifiedLocation();
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return syntaxMessage;
    }
  }
}
