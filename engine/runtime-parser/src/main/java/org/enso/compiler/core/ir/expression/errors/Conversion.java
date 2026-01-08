package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic$Kind$Interactive;
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

/** An error resulting from processing conversion methods. */
@GenerateIR(
    interfaces = {
      Error.class,
      IRKind.Primitive.class,
      Name.class,
      Diagnostic$Kind$Interactive.class
    })
public final class Conversion extends ConversionGen {
  @GenerateFields
  public Conversion(@IRChild IR storedIR, @IRField Reason reason, MetadataStorage passData) {
    super(storedIR, reason, passData);
  }

  public static Conversion create(IR storedIR, Reason reason) {
    return builder().storedIR(storedIR).reason(reason).build();
  }

  @Override
  public String showCode(int indent) {
    return "(Error: " + storedIR().showCode(indent) + ")";
  }

  @Override
  public String message(Function1<IdentifiedLocation, String> source) {
    return reason().explain();
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {reason().explain()};
  }

  @Override
  public Conversion mapExpressions(Function<Expression, Expression> fn) {
    return this;
  }

  @Override
  public String name() {
    return "conversion_error";
  }

  public sealed interface Reason {
    String explain();
  }

  public static final class DeclaredAsPrivate implements Reason {
    private DeclaredAsPrivate() {}

    public static final DeclaredAsPrivate INSTANCE = new DeclaredAsPrivate();

    @Override
    public String explain() {
      return "Conversion methods cannot be private";
    }
  }

  public static final class MissingArgs implements Reason {
    private MissingArgs() {}

    public static final MissingArgs INSTANCE = new MissingArgs();

    @Override
    public String explain() {
      return "A conversion definition must have at least one argument.";
    }
  }

  public static final class UnsupportedSourceType implements Reason {
    private UnsupportedSourceType() {}

    public static final UnsupportedSourceType INSTANCE = new UnsupportedSourceType();

    @Override
    public String explain() {
      return "Arbitrary expressions are not yet supported as source types.";
    }
  }

  public record MissingSourceType(String argName) implements Reason {
    @Override
    public String explain() {
      return "The argument `" + argName + "` does not define a source type.";
    }
  }

  public record NonDefaultedArgument(String argName) implements Reason {
    @Override
    public String explain() {
      return "Additional arguments in a conversion must have a default, but "
          + "`"
          + argName
          + "` does not.";
    }
  }

  public record SuspendedSourceArgument(String argName) implements Reason {
    @Override
    public String explain() {
      return "The `that` type argument in a conversion (here " + argName + ") cannot be suspended.";
    }
  }

  public record InvalidSourceArgumentName(String argName) implements Reason {
    @Override
    public String explain() {
      return "The source type argument must be ignored or named `that`, but "
          + argName
          + " was found.";
    }
  }
}
