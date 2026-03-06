package org.enso.compiler.core.ir.expression;

import java.util.function.Function;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import scala.Option;

public interface Operator extends Application {
  @Override
  Operator mapExpressions(Function<Expression, Expression> fn);

  @Override
  Operator setLocation(Option<IdentifiedLocation> location);

  @Override
  Operator duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /** A representation of a generic binary operator application in Enso. */
  @GenerateIR(interfaces = {Operator.class, IRKind.Sugar.class})
  final class Binary extends OperatorBinaryGen {
    /**
     * @param left the left operand to `operator`
     * @param operator the operator function being called
     * @param right the right operand to `operator`
     */
    @GenerateFields
    public Binary(
        @IRChild CallArgument left,
        @IRChild Name operator,
        @IRChild CallArgument right,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(left, operator, right, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String showCode(int indent) {
      var opStr = operator().showCode(indent);
      return "((" + left().showCode(indent) + ") " + opStr + " (" + right().showCode(indent) + "))";
    }
  }
}
