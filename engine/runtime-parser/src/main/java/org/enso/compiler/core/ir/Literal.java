package org.enso.compiler.core.ir;

import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;

import java.util.function.Function;

public interface Literal extends Expression, IRKind.Primitive {

  @Override
  Literal mapExpressions(Function<Expression, Expression> fn);

  @Override
  Literal setLocation(Option<IdentifiedLocation> location);

  @Override
  Literal duplicate(boolean keepLocations, boolean keepMetadata, boolean keepDiagnostics, boolean keepIdentifiers);

  @GenerateIR(interfaces = {Literal.class})
  final class Number extends LiteralNumberGen {
    @GenerateFields
    public Number(
        @IRField Option<String> base,
        @IRField String value,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData
    ) {
      super(base, value, identifiedLocation, passData);
    }

    /** Checks whether the literal represents a fractional value.
     */
    public boolean isFractional() {
      return value().contains(".");
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
        @IRField String text,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData
    ) {
      super(text, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      var tripleQuotes = "\"\"\"";
      return tripleQuotes + text() + tripleQuotes;
    }
  }
}
