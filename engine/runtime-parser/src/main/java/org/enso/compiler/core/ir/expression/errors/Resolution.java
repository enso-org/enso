package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.ir.Diagnostic;
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
import scala.Option;

/** A representation of an erro resulting from name resolution. */
@GenerateIR(
    interfaces = {
      Error.class,
      Diagnostic.Kind.Interactive.class,
      IRKind.Primitive.class,
      Name.class
    })
public final class Resolution extends ResolutionErrorGen {

  /**
   * @param originalName the original name that could not be resolved.
   */
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
    return message(source) + ".";
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {reason()};
  }

  @Override
  public Resolution setLocation(Option<IdentifiedLocation> location) {
    var newOrigName = originalName().setLocation(location);
    return builder(this).originalName(newOrigName).build();
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return originalName().identifiedLocation();
  }

  @Override
  public Option<IdentifiedLocation> location() {
    return Option.apply(identifiedLocation());
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

  /** An error coming from a tail call annotation placed in a syntactically incorrect position. */
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

  /**
   * An error coming from an unexpected occurence of a polyglot symbol.
   *
   * @param context the description of a context in which the error happened.
   */
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

  /**
   * An error coming from an unexpected occurence of a constructor.
   *
   * @param context the description of a context in which the error happened.
   */
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

  /**
   * An error coming from an unexpected occurence of a static method.
   *
   * @param context the description of a context in which the error happened.
   */
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

  /**
   * An error coming from an unexpected occurence of a module.
   *
   * @param context the description of a context in which the error happened.
   */
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

  /**
   * An error when a project-private entity (module, type, method) is used from a different project.
   *
   * @param callerProject Name of the project of caller.
   * @param calleeProject Name of the project of callee.
   */
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

  /** An error coming from name resolver. */
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
