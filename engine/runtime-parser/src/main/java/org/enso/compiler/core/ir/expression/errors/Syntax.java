package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.ir.Diagnostic$Kind$Interactive;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.expression.Error;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Function1;
import scala.Option;

/** A representation of an Enso syntax error. */
@GenerateIR(
    interfaces = {
      Error.class,
      Diagnostic$Kind$Interactive.class,
      IRKind.Primitive.class,
      Definition.class,
      Import.class,
      Export.class
    })
public final class Syntax extends SyntaxErrorGen {
  @GenerateFields
  public Syntax(
      @IRField Reason reason,
      IdentifiedLocation identifiedLocation,
      MetadataStorage passData,
      DiagnosticStorage diagnostics) {
    super(reason, identifiedLocation, passData, diagnostics);
  }

  public static Syntax create(IdentifiedLocation at, Reason reason) {
    return builder().reason(reason).location(at).build();
  }

  public static Builder builder() {
    return new Builder();
  }

  @Override
  public Syntax setLocation(Option<IdentifiedLocation> location) {
    return this;
  }

  @Override
  public Syntax mapExpressions(Function<Expression, Expression> fn) {
    return this;
  }

  @Override
  public String message(Function1<IdentifiedLocation, String> source) {
    return reason().explanation();
  }

  @Override
  public String formattedMessage(Function1<IdentifiedLocation, String> source) {
    return message(source) + ".";
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {reason()};
  }

  @Override
  public String showCode(int indent) {
    return "Syntax_Error";
  }

  public sealed interface Reason {
    String explanation();
  }

  public record InvalidEscapeSequence(String lit) implements Reason {
    @Override
    public String explanation() {
      return "Invalid escape sequence " + lit;
    }
  }

  public record UnsupportedSyntax(String explanation) implements Reason {
    @Override
    public String explanation() {
      return explanation;
    }
  }

  public static final class IncosistentConstructorVisibility implements Reason {
    private IncosistentConstructorVisibility() {}

    public static final IncosistentConstructorVisibility INSTANCE =
        new IncosistentConstructorVisibility();

    @Override
    public String explanation() {
      return "Private and public constructors cannot be mixed within a single type";
    }
  }

  public record InvalidImport(String message) implements Reason {
    @Override
    public String explanation() {
      return "Invalid Import: " + message;
    }
  }

  public record InvalidExport(String message) implements Reason {
    @Override
    public String explanation() {
      return "Invalid Export: " + message;
    }
  }

  public static final class UnexpectedDeclarationInType implements Reason {
    private UnexpectedDeclarationInType() {}

    public static final UnexpectedDeclarationInType INSTANCE = new UnexpectedDeclarationInType();

    @Override
    public String explanation() {
      return "Unexpected declaration in the body of a type";
    }
  }

  public static final class EmptyParentheses implements Reason {
    private EmptyParentheses() {}

    public static final EmptyParentheses INSTANCE = new EmptyParentheses();

    @Override
    public String explanation() {
      return "Parentheses can't be empty";
    }
  }

  public static final class UnexpectedExpression implements Reason {
    private UnexpectedExpression() {}

    public static final UnexpectedExpression INSTANCE = new UnexpectedExpression();

    @Override
    public String explanation() {
      return "Unexpected expression";
    }
  }

  public static final class AmbiguousExpression implements Reason {
    private AmbiguousExpression() {}

    public static final AmbiguousExpression INSTANCE = new AmbiguousExpression();

    @Override
    public String explanation() {
      return "Ambiguous expression";
    }
  }

  public static final class InvalidSelfArgUsage implements Reason {
    private InvalidSelfArgUsage() {}

    public static final InvalidSelfArgUsage INSTANCE = new InvalidSelfArgUsage();

    @Override
    public String explanation() {
      return "Self argument cannot be used in static methods";
    }
  }

  public static final class UnrecognizedToken implements Reason {
    private UnrecognizedToken() {}

    public static final UnrecognizedToken INSTANCE = new UnrecognizedToken();

    @Override
    public String explanation() {
      return "Unrecognized token";
    }
  }

  public static final class UnclosedTextLiteral implements Reason {
    private UnclosedTextLiteral() {}

    public static final UnclosedTextLiteral INSTANCE = new UnclosedTextLiteral();

    @Override
    public String explanation() {
      return "Unclosed text literal";
    }
  }

  public static final class InvalidOperator implements Reason {
    private InvalidOperator() {}

    public static final InvalidOperator INSTANCE = new InvalidOperator();

    @Override
    public String explanation() {
      return "Operator must have two arguments";
    }
  }

  public record InvalidForeignDefinition(String details) implements Reason {

    @Override
    public String explanation() {
      return "Invalid foreign definition. " + details;
    }
  }
}
