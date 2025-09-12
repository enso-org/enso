package org.enso.compiler.core.ir.expression;

import java.util.function.Function;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;

/** Foreign code entities. */
public interface Foreign extends Expression {

  @Override
  Foreign mapExpressions(Function<Expression, Expression> fn);

  @Override
  Foreign duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @Override
  Foreign setLocation(Option<IdentifiedLocation> location);

  /** Foreign code definition in Enso. */
  @GenerateIR(interfaces = {Foreign.class, IRKind.Primitive.class})
  final class Definition extends ForeignDefinitionGen {

    /**
     * @param lang the foreign language being written
     * @param code the code written in `lang`
     */
    @GenerateFields
    public Definition(
        @IRField String lang,
        @IRField String code,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(lang, code, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    @Override
    public String showCode(int indent) {
      return "FOREIGN DEF";
    }
  }
}
