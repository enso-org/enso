package org.enso.compiler.core.ir.expression;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.Diagnostic$Kind$Static;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import scala.Function1;
import scala.Option;

public interface Error extends Expression, Definition, Diagnostic {

  @Override
  Error mapExpressions(Function<Expression, Expression> fn);

  @Override
  Error setLocation(Option<IdentifiedLocation> location);

  @Override
  Error duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @Override
  default Option<IdentifiedLocation> location() {
    return Expression.super.location();
  }

  @GenerateIR(interfaces = {Error.class, IRKind.Primitive.class})
  final class InvalidIR extends ErrorInvalidIRGen implements Diagnostic$Kind$Static {
    @GenerateFields
    public InvalidIR(@IRChild IR ir, MetadataStorage passData) {
      super(ir, passData);
    }

    public static InvalidIR create(IR ir) {
      return builder().ir(ir).build();
    }

    @Override
    public String showCode(int indent) {
      return "Invalid_Ir";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[0];
    }

    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "InvalidIR: Please report this as a compiler bug.";
    }
  }
}
