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

public interface Section extends Operator {
  @Override
  Section mapExpressions(Function<Expression, Expression> fn);

  @Override
  Section setLocation(Option<IdentifiedLocation> location);

  @Override
  Section duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /** Represents a left operator section of the form `(arg op)`. */
  @GenerateIR(interfaces = {Section.class, IRKind.Sugar.class})
  final class Left extends SectionLeftGen {
    /**
     * @param arg the argument (on the left of the operator)
     * @param operator the operator
     */
    @GenerateFields
    public Left(
        @IRChild CallArgument arg,
        @IRChild Name operator,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(arg, operator, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "(" + arg().showCode(indent) + " " + operator().showCode(indent) + ")";
    }
  }

  /** Represents a sides operator section of the form `(op)` */
  @GenerateIR(interfaces = {Section.class, IRKind.Sugar.class})
  final class Sides extends SectionSidesGen {
    @GenerateFields
    public Sides(
        @IRChild Name operator, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(operator, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "(" + operator().showCode(indent) + ")";
    }
  }

  /** Represents a right operator section of the form `(op arg)`. */
  @GenerateIR(interfaces = {Section.class, IRKind.Sugar.class})
  final class Right extends SectionRightGen {
    /**
     * @param arg the argument (on the right of the operator)
     * @param operator the operator
     */
    @GenerateFields
    public Right(
        @IRChild Name operator,
        @IRChild CallArgument arg,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(operator, arg, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "(" + operator().showCode(indent) + " " + arg().showCode(indent) + ")";
    }
  }
}
