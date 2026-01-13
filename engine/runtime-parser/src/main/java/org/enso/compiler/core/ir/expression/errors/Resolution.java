package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.ir.Diagnostic$Kind$Interactive;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.Error;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Function1;

@GenerateIR(
    interfaces = {
      Error.class,
      Diagnostic$Kind$Interactive.class,
      IRKind.Primitive.class,
      Name.class
    })
public final class Resolution extends ResolutionErrorGen {
  @GenerateFields
  public Resolution(
      @IRChild Name originalName,
      @IRField Reason reason,
      MetadataStorage passData,
      DiagnosticStorage diagnostics) {
    super(originalName, reason, passData, diagnostics);
  }

  public static Resolution create(Name originalName, Reason reason) {
    return builder().originalName(originalName).reason(reason).build();
  }

  @Override
  public Resolution mapExpressions(Function<Expression, Expression> fn) {
    return this;
  }

  @Override
  public String name() {
    return originalName().name();
  }

  @Override
  public String showCode(int indent) {
    return originalName().showCode(indent);
  }

  @Override
  public String message(Function1<IdentifiedLocation, String> source) {
    return reason().explain(originalName());
  }

  @Override
  public String formattedMessage(Function1<IdentifiedLocation, String> source) {
    return message(source);
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {reason()};
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return originalName().identifiedLocation();
  }

  sealed interface Reason {
    String explain(Name originalName);
  }

  public static final class UnknownAnnotation implements Reason {
    private UnknownAnnotation() {}

    public static final UnknownAnnotation INSTANCE = new UnknownAnnotation();

    @Override
    public String explain(Name originalName) {
      return "The annotation " + originalName.name() + " is not defined";
    }
  }

  public static final class UnexpectedAnnotation implements Reason {
    private UnexpectedAnnotation() {}

    public static final UnexpectedAnnotation INSTANCE = new UnexpectedAnnotation();

    @Override
    public String explain(Name originalName) {
      return "Unexpected "
          + originalName.name()
          + " annotation. This annotation can "
          + "only be used with function applications";
    }
  }

  public record UnexpectedPolyglot(String context) implements Reason {
    @Override
    public String explain(Name originalName) {
      return "The name "
          + originalName.name()
          + " resolved to a polyglot symbol, "
          + "but polyglot symbols are not allowed in "
          + context;
    }
  }

  public record UnexpectedConstructor(String context) implements Reason {
    @Override
    public String explain(Name originalName) {
      return "The name "
          + originalName.name()
          + " resolved to a constructor, "
          + "but constructors are not allowed in "
          + context;
    }
  }

  public record UnexpectedMethod(String context) implements Reason {
    @Override
    public String explain(Name originalName) {
      return "The name "
          + originalName.name()
          + " resolved to a method, "
          + "but methods are not allowed in "
          + context;
    }
  }

  public record UnexpectedModule(String context) implements Reason {
    @Override
    public String explain(Name originalName) {
      return "The name "
          + originalName.name()
          + " resolved to a module, "
          + "but modules are not allowed in "
          + context;
    }
  }

  public record PrivateEntity(String callerProject, String calleeProject) implements Reason {
    @Override
    public String explain(Name originalName) {
      return "Project-private entity '"
          + originalName.name()
          + "' in project '"
          + calleeProject
          + "' cannot be used from project '"
          + callerProject
          + "'";
    }
  }

  public record ResolverError(ExplainResolution explain) implements Reason {

    @Override
    public String explain(Name originalName) {
      return explain.explain(originalName);
    }
  }

  public interface ExplainResolution {
    String explain(Name originalName);
  }

  public record MissingLibraryImportInFQNError(String namespace) implements Reason {

    @Override
    public String explain(Name originalName) {
      return "Fully qualified name references a library "
          + namespace
          + "."
          + originalName.name()
          + " but an import statement for it is missing";
    }
  }
}
