package org.enso.compiler.core.ir.expression.errors;

import org.enso.compiler.core.ir.Diagnostic$Kind$Interactive;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.expression.Error;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Function1;
import scala.Option;

/** A representation of an error resulting from wrong pattern matches. */
@GenerateIR(
    interfaces = {
      Error.class,
      Diagnostic$Kind$Interactive.class,
      IRKind.Primitive.class,
      org.enso.compiler.core.ir.Pattern.class
    })
public final class Pattern extends ErrorPatternGen {

  /**
   * @param originalPattern pattern that resulted in the error
   * @param reason the cause of this error
   */
  @GenerateFields
  public Pattern(
      @IRChild org.enso.compiler.core.ir.Pattern originalPattern,
      @IRField Reason reason,
      MetadataStorage passData,
      DiagnosticStorage diagnostics) {
    super(originalPattern, reason, passData, diagnostics);
  }

  public static Pattern create(org.enso.compiler.core.ir.Pattern originalPattern, Reason reason) {
    return builder().originalPattern(originalPattern).reason(reason).build();
  }

  @Override
  public String message(Function1<IdentifiedLocation, String> source) {
    return reason().explain();
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {reason()};
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return originalPattern().identifiedLocation();
  }

  @Override
  public Pattern setLocation(Option<IdentifiedLocation> location) {
    var newOrigPattern = originalPattern().setLocation(location);
    return builder(this).originalPattern(newOrigPattern).build();
  }

  @Override
  public String showCode(int indent) {
    return originalPattern().showCode(indent);
  }

  public sealed interface Reason {
    String explain();
  }

  /**
   * A reason for pattern failing due to wrong arity.
   *
   * @param consName the constructor name.
   * @param expected expected field count.
   * @param actual actual field count.
   */
  public record WrongArity(String consName, int expected, int actual) implements Reason {

    @Override
    public String explain() {
      return "Wrong number of fields when matching on "
          + consName
          + ". Expected "
          + expected
          + " fields, but provided "
          + actual;
    }
  }

  /**
   * An error when a project-private constructor is used in the pattern.
   *
   * @param consName Name of the constructor. Does not have to be fully qualified.
   * @param callerProject The project name of the caller.
   * @param calleeProject The project name of the callee. The constructor is in this project.
   */
  public record PrivateConstructor(String consName, String callerProject, String calleeProject)
      implements Reason {

    @Override
    public String explain() {
      return "Project-private constructor '"
          + consName
          + "' in project '"
          + calleeProject
          + "' cannot be used from project '"
          + callerProject
          + "'";
    }
  }
}
