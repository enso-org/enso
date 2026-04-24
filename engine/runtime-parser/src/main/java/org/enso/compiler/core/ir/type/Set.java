package org.enso.compiler.core.ir.type;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Type;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import scala.Option;
import scala.collection.immutable.List;

/** IR nodes for dealing with typesets. */
public interface Set extends Type {

  @Override
  Set mapExpressions(java.util.function.Function<Expression, Expression> fn);

  @Override
  Set setLocation(Option<IdentifiedLocation> location);

  @Override
  Set duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /** The representation of a typeset member. */
  @GenerateIR(interfaces = {Set.class, IRKind.Primitive.class})
  final class Member extends SetMemberGen {
    /**
     * @param label the member's label, if given
     * @param memberType the member's type, if given
     * @param value the member's value, if given
     * @param identifiedLocation the source location that the node corresponds to
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Member(
        @IRChild Name label,
        @IRChild Expression memberType,
        @IRChild Expression value,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(label, memberType, value, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      var typeString = " : " + memberType().showCode(indent);
      var valueString = " = " + value().showCode(indent);
      return "(" + label().showCode(indent) + typeString + valueString + ")";
    }
  }

  /** The typeset union operator {@code |}. */
  @GenerateIR(interfaces = {Set.class, IRKind.Primitive.class})
  final class Union extends SetUnionGen {
    public static final String NAME = "|";

    @GenerateFields
    public Union(
        @IRChild List<Expression> operands,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(operands, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Union copyWithOperands(List<Expression> newOperands) {
      return new Builder(this).operands(newOperands).build();
    }

    @Override
    public String showCode(int indent) {
      return operands().map(op -> op.showCode(indent)).toList().mkString(" | ");
    }
  }

  /** The typeset intersection operator {@code &}. */
  @GenerateIR(interfaces = {Set.class, IRKind.Primitive.class})
  final class Intersection extends SetIntersectionGen {
    public static final String NAME = "&";

    @GenerateFields
    public Intersection(
        @IRChild Expression left,
        @IRChild Expression right,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(left, right, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      return "(" + left().showCode(indent) + " & " + right().showCode(indent) + ")";
    }
  }
}
