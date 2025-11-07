package org.enso.compiler.core.ir.module.scope;

import java.util.function.Function;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.module.Scope;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface Export extends Scope {

  @Override
  Export mapExpressions(Function<Expression, Expression> fn);

  @Override
  Export setLocation(Option<IdentifiedLocation> location);

  @Override
  Export duplicate(boolean keepLocations, boolean keepMetadata, boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Export.class, IRKind.Primitive.class})
  final class Module extends ExportModuleGen {
    @GenerateFields
    public Module(
        @IRChild Name.Qualified name,
        @IRChild(required = false) Name.Literal rename,
        @IRChild(required = false) List<Name.Literal> onlyNames,
        @IRField boolean isSynthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData
    ) {
      super(
          name,
          rename,
          onlyNames,
          isSynthetic,
          identifiedLocation,
          passData);
    }

    public static Builder builder() {
      return new Builder().isSynthetic(false);
    }

    @Override
    public Module mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }

    @Override
    public String showCode(int indent) {
      var renameCode =
          rename() != null ? " as " + rename().name() : "";
      if (onlyNames() != null) {
        return "from "
            + name().name()
            + " export "
            + onlyNames().map(Literal::name).mkString(", ")
            + renameCode;
      } else {
        return "export "
            + name().name()
            + renameCode;
      }
    }
  }
}
