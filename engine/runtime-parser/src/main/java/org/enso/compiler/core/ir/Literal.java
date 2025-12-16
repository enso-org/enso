package org.enso.compiler.core.ir;

import java.math.BigInteger;
import java.util.function.Function;
import org.enso.compiler.core.CompilerError;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;

public interface Literal extends Expression, IRKind.Primitive {

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

  @GenerateIR(interfaces = {Literal.class})
  final class Number extends LiteralNumberGen {
    @GenerateFields
    public Number(
        @IRField Option<String> base,
        @IRField String value,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(base, value, identifiedLocation, passData);
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    /** Checks whether the literal represents a fractional value. */
    public boolean isFractional() {
      return value().contains(".");
    }

    /**
     * Checks the values in the literal converts that to approviate JVM value.
     *
     * @return Double, Long, BigInteger
     */
    public Object numericValue() {
      if (isFractional()) {
        return Double.parseDouble(value());
      }
      if (base().isDefined()) {
        int baseNum;
        try {
          baseNum = Integer.parseInt(base().get());
        } catch (NumberFormatException e) {
          throw new CompilerError("Invalid number base " + base().get() + " seen during codegen.");
        }
        try {
          return Long.parseLong(value(), baseNum);
        } catch (NumberFormatException e) {
          try {
            return new BigInteger(value(), baseNum);
          } catch (NumberFormatException e2) {
            throw new CompilerError(
                "Invalid number base " + base().get() + " seen during codegen.");
          }
        }
      }
      try {
        return Long.parseLong(value());
      } catch (NumberFormatException e) {
        return new BigInteger(value());
      }
    }

    @Override
    public String showCode(int indent) {
      if (base().isDefined()) {
        return base().get() + "_" + value();
      } else {
        return value();
      }
    }
  }

  @GenerateIR(interfaces = {Literal.class})
  final class Text extends LiteralTextGen {
    @GenerateFields
    public Text(
        @IRField String text, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(text, identifiedLocation, passData);
    }

    public static Text fromString(String str) {
      return builder().text(str).build();
    }

    @Override
    public String showCode(int indent) {
      return "\"" + text() + "\"";
    }
  }
}
