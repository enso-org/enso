package org.enso.compiler.core.ir.expression;

import java.util.function.Function;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface Application extends Expression {

  /** A standard prefix function application. */
  @GenerateIR(interfaces = {Application.class, IRKind.Primitive.class})
  final class Prefix extends ApplicationPrefixGen {
    @GenerateFields
    public Prefix(
        @IRChild Expression function,
        @IRChild List<CallArgument> arguments,
        @IRField boolean hasDefaultsSuspended,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(function, arguments, hasDefaultsSuspended, identifiedLocation, passData, diagnostics);
    }

    public Prefix(
        Expression function,
        List<CallArgument> arguments,
        boolean hasDefaultsSuspended,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      this(function, arguments, hasDefaultsSuspended, identifiedLocation, passData, null);
    }

    public Prefix(Expression function, List<CallArgument> arguments) {
      this(function, arguments, false, null, new MetadataStorage(), null);
    }

    public Prefix copy(Expression function, List<CallArgument> arguments) {
      return copy(
          diagnostics(),
          passData(),
          identifiedLocation(),
          id,
          function,
          arguments,
          hasDefaultsSuspended());
    }

    public Prefix copyWithArguments(List<CallArgument> arguments) {
      return copy(
          diagnostics(),
          passData(),
          identifiedLocation(),
          id,
          function(),
          arguments,
          hasDefaultsSuspended());
    }

    public Prefix copyWithFunction(Expression function) {
      return copy(
          diagnostics(),
          passData(),
          identifiedLocation(),
          id,
          function,
          arguments(),
          hasDefaultsSuspended());
    }

    @Override
    public String showCode(int indent) {
      var argStr = arguments().map(arg -> arg.showCode(indent)).mkString(" ");
      var funcStr = function().showCode(indent);
      return "((" + funcStr + ") " + argStr + ")";
    }
  }

  /** A representation of a term that is explicitly forced. */
  @GenerateIR(interfaces = {Application.class, IRKind.Primitive.class})
  final class Force extends ApplicationForceGen {
    /**
     * @param target The expression being forced
     * @param identifiedLocation
     * @param passData
     */
    @GenerateFields
    public Force(
        @IRChild Expression target,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(target, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "(FORCE " + target().showCode(indent) + ")";
    }

    public Force copyWithTarget(Expression target) {
      return copy(diagnostics(), passData(), identifiedLocation(), id, target);
    }
  }

  /** Literal applications in Enso. */
  interface Literal extends Application {
    @Override
    Literal mapExpressions(Function<Expression, Expression> fn);

    @Override
    Literal setLocation(Option<IdentifiedLocation> location);

    @Override
    Literal duplicate(
        boolean keepLocations,
        boolean keepMetadata,
        boolean keepDiagnostics,
        boolean keepIdentifiers);
  }

  /**
   * A representation of a typeset literal.
   *
   * <p>These are necessary as they delimit pattern contexts.
   */
  @GenerateIR(interfaces = {Literal.class, IRKind.Primitive.class})
  final class Typeset extends ApplicationTypesetGen {

    /**
     * @param expression The expression of the typeset body.
     */
    @GenerateFields
    public Typeset(
        @IRChild Option<Expression> expression,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(expression, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      String exprString;
      if (expression().isDefined()) {
        exprString = expression().get().showCode(indent);
      } else {
        exprString = "";
      }
      return "{" + exprString + "}";
    }

    public Typeset copyWithExpression(Option<Expression> expression) {
      return copy(diagnostics(), passData(), identifiedLocation(), id, expression);
    }
  }

  /** A representation of a vector literal. */
  @GenerateIR(interfaces = {Literal.class, IRKind.Primitive.class})
  final class Sequence extends ApplicationSequenceGen {

    /**
     * @param items The items being put in the vector.
     */
    @GenerateFields
    public Sequence(
        @IRChild List<Expression> items,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(items, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      var itemsStr = items().map(it -> it.showCode(indent)).mkString(" ");
      return "[" + itemsStr + "]";
    }

    public Sequence copyWithItems(List<Expression> items) {
      return copy(diagnostics(), passData(), identifiedLocation(), id, items);
    }
  }
}
