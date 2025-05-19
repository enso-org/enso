package org.enso.compiler.core.ir.module.scope;

import java.util.function.Function;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.Name.Qualified;
import org.enso.compiler.core.ir.module.Scope;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface Import extends Scope {

  @Override
  Import mapExpressions(Function<Expression, Expression> fn);

  @Override
  Import setLocation(Option<IdentifiedLocation> location);

  @Override
  Import duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Import.class, IRKind.Primitive.class})
  final class Module extends ImportModuleGen {
    @GenerateFields
    public Module(
        @IRChild Qualified name,
        @IRChild Option<Literal> rename,
        @IRField boolean isAll,
        @IRChild Option<List<Literal>> onlyNames,
        @IRChild Option<List<Literal>> hiddenNames,
        @IRField boolean isSynthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(
          name,
          rename,
          isAll,
          onlyNames,
          hiddenNames,
          isSynthetic,
          identifiedLocation,
          passData,
          diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public static Builder builder(Module module) {
      return new Builder(module);
    }

    public Module(
        Qualified name,
        Option<Literal> rename,
        boolean isAll,
        Option<List<Literal>> onlyNames,
        Option<List<Literal>> hiddenNames,
        boolean isSynthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      this(
          name,
          rename,
          isAll,
          onlyNames,
          hiddenNames,
          isSynthetic,
          identifiedLocation,
          passData,
          null);
    }

    public static Module createSynthetic(Name.Qualified name) {
      return new Module(
          name,
          Option.empty(),
          false,
          Option.empty(),
          Option.empty(),
          true,
          null,
          new MetadataStorage(),
          null);
    }

    public Module copyWithNameAndRename(Name.Qualified name, Option<Name.Literal> rename) {
      return new Builder(this).name(name).rename(rename).build();
    }

    public Module copyWithName(Name.Qualified name) {
      return new Builder(this).name(name).build();
    }

    @Override
    public String showCode(int indent) {
      var renameCode = "";
      if (rename().isDefined()) {
        renameCode = " as " + rename().get().name();
      }
      if (isAll()) {
        var onlyPart = "";
        if (onlyNames().isDefined()) {
          onlyPart = " " + onlyNames().get().map(Literal::name).mkString(", ");
        }
        var hidingPart = "";
        if (hiddenNames().isDefined()) {
          hidingPart = " hiding " + hiddenNames().get().map(Literal::name).mkString(", ");
        }
        var all = onlyNames().isDefined() ? "" : " all";
        return "from " + name().name() + renameCode + " import" + onlyPart + all + hidingPart;
      } else {
        return "import " + name().name() + renameCode;
      }
    }

    /**
     * Gets the name of the module visible in this scope, either the original name or the rename.
     *
     * @return the name of this import visible in code
     */
    public Name getSimpleName() {
      if (rename().isDefined()) {
        return rename().get();
      } else {
        return name().parts().last();
      }
    }

    /**
     * Checks whether the import statement allows use of the given exported name.
     *
     * <p>Note that it does not verify if the name is actually exported by the module, only checks
     * if it is syntactically allowed.
     *
     * @param name the name to check
     * @return whether the name could be accessed or not
     */
    public boolean allowsAccess(String name) {
      if (!isAll()) {
        return false;
      }
      if (onlyNames().isDefined()) {
        return onlyNames().get().exists(nm -> nm.name().equals(name));
      } else if (hiddenNames().isDefined()) {
        return !hiddenNames().get().exists(nm -> nm.name().equals(name));
      } else {
        return true;
      }
    }
  }
}
