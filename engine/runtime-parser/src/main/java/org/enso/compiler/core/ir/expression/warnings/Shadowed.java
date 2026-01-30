package org.enso.compiler.core.ir.expression.warnings;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Warning;
import scala.Function1;

/** Warnings about shadowing names. */
public sealed interface Shadowed extends Warning
    permits Shadowed.FunctionParam,
        Shadowed.PatternBinding,
        Shadowed.SyntheticModule,
        Shadowed.TypeInModuleNameConflicts {

  /** The {@link IR} shadowing the warned expression. */
  IR shadower();

  /**
   * A warning that a later-defined lambda parameter shadows an earlier-defined lambda parameter.
   *
   * @param shadowedName the name being shadowed
   * @param shadower the expression shadowing {@code warnedExpr}
   * @param identifiedLocation the location at which the shadowing takes place
   */
  record FunctionParam(String shadowedName, IR shadower, IdentifiedLocation identifiedLocation)
      implements Shadowed {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "The argument '" + shadowedName + "' is shadowed by another one with the same name.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {shadowedName, shadower};
    }
  }

  /**
   * A warning that a later-defined pattern variable shadows an earlier-defined pattern variable.
   *
   * @param shadowedName the name being shadowed
   * @param shadower the expression shadowing {@code warnedExpr}
   * @param identifiedLocation the location at which the shadowing takes place
   */
  record PatternBinding(String shadowedName, IR shadower, IdentifiedLocation identifiedLocation)
      implements Shadowed {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "The pattern field '"
          + shadowedName
          + "' is shadowed by another one with the same name.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {shadowedName, shadower};
    }
  }

  /**
   * A warning that a submodule is being shadowed by the type of the same name therefore preventing
   * the user from accessing the module via a qualified name.
   *
   * @param typeName the type name shadowing the module
   * @param moduleName the module being shadowed
   * @param shadower the expression shadowing {@code moduleName}
   * @param identifiedLocation the location at which the shadowing takes place
   */
  record SyntheticModule(
      String typeName,
      Name.Qualified moduleName,
      IR shadower,
      IdentifiedLocation identifiedLocation)
      implements Shadowed {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "Declaration of type "
          + typeName
          + " shadows module "
          + moduleName.name()
          + " making it inaccessible via a qualified name.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {typeName, moduleName, shadower};
    }
  }

  /**
   * Used when the exported type of the module can name conflict with fully qualified names of
   * submodules.
   *
   * @param name the module name
   * @param tpeName the name of the exported type leading to conflicts
   * @param firstConflict the name of the module that can be inaccessible because of the name
   *     conflict
   * @param shadower the export statement leading to a conflict
   * @param identifiedLocation the location of the export statement
   */
  record TypeInModuleNameConflicts(
      String name,
      String tpeName,
      String firstConflict,
      IR shadower,
      IdentifiedLocation identifiedLocation)
      implements Shadowed {
    @Override
    public String message(Function1<IdentifiedLocation, String> source) {
      return "The exported type `"
          + tpeName
          + "` in `"
          + name
          + "` module will cause name conflict "
          + "when attempting to use a fully qualified name of the `"
          + firstConflict
          + "` module.";
    }

    @Override
    public Object[] diagnosticKeys() {
      return new Object[] {name, tpeName, shadower};
    }
  }
}
