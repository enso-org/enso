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

/** An export statement */
public interface Export extends Scope {

  @Override
  Export mapExpressions(Function<Expression, Expression> fn);

  @Override
  Export setLocation(Option<IdentifiedLocation> location);

  @Override
  Export duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Export.class, IRKind.Primitive.class})
  final class Module extends ExportModuleGen {

    /**
     * @param name the full path representing the export
     * @param rename the name this export is visible as
     * @param onlyNames exported names selected from the exported module
     * @param identifiedLocation the source location that the node corresponds to
     * @param isSynthetic is this export compiler-generated
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Module(
        @IRChild Name.Qualified name,
        @IRChild Option<Name.Literal> rename,
        @IRChild Option<List<Name.Literal>> onlyNames,
        @IRField boolean isSynthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, rename, onlyNames, isSynthetic, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().rename(Option.empty()).onlyNames(Option.empty()).isSynthetic(false);
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    public Module copyWithName(Name.Qualified name) {
      return new Builder(this).name(name).build();
    }

    @Override
    public Module mapExpressions(Function<Expression, Expression> fn) {
      return this;
    }

    /**
     * Gets the name of the module visible in the importing scope, either the original name or the
     * rename.
     *
     * @return the name of this export visible in code
     */
    public Name getSimpleName() {
      if (rename().isDefined()) {
        return rename().get();
      } else {
        return name().parts().last();
      }
    }

    /**
     * Checks whether the export statement allows use of the given exported name.
     *
     * <p>Note that it does not verify if the name is actually exported by the module, only checks
     * if it is syntactically allowed.
     *
     * @param name the name to check
     * @return whether the name could be accessed or not
     */
    public boolean allowsAccess(String name) {
      if (onlyNames().isDefined()) {
        return onlyNames().get().exists(n -> n.name().equalsIgnoreCase(name));
      } else {
        return true;
      }
    }

    @Override
    public String showCode(int indent) {
      var renameCode = rename().isDefined() ? " as " + rename().get().name() : "";
      if (onlyNames().isDefined()) {
        return "from "
            + name().name()
            + " export "
            + onlyNames().get().map(Literal::name).mkString(", ")
            + renameCode;
      } else {
        return "export " + name().name() + renameCode;
      }
    }
  }
}
