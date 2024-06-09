package org.enso.compiler.core.ir
package expression
package warnings

import org.enso.compiler.core.IR

/** Warnings about shadowing names. */
sealed trait Shadowed extends Warning {

  /** The [[IR]] shadowing the warned expression. */
  val shadower: IR
}

object Shadowed {

  /** A warning that a later-defined lambda parameter shadows an
    * earlier-defined lambda parameter.
    *
    * @param shadowedName the name being shadowed
    * @param shadower     the expression shadowing `warnedExpr`
    * @param location     the location at which the shadowing takes place
    */
  sealed case class FunctionParam(
    shadowedName: String,
    override val shadower: IR,
    override val location: Option[IdentifiedLocation]
  ) extends Shadowed {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The argument '$shadowedName' is shadowed by another one with the same name."

    override def diagnosticKeys(): Array[Any] =
      Array(shadowedName, shadower)
  }

  /** A warning that a later-defined pattern variable shadows an
    * earlier-defined pattern variable.
    *
    * @param shadowedName the name being shadowed
    * @param shadower     the expression shadowing `warnedExpr`
    * @param location     the location at which the shadowing takes place
    */
  sealed case class PatternBinding(
    shadowedName: String,
    override val shadower: IR,
    override val location: Option[IdentifiedLocation]
  ) extends Shadowed {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The pattern field '$shadowedName' is shadowed by another one with the same name."

    override def diagnosticKeys(): Array[Any] =
      Array(shadowedName, shadower)
  }

  /** A warning that a submodule is being shadowed by the type of the same name
    * therefore preventing the user from accessing the module via a qualified name.
    *
    * @param typename   the type name shadowing the module
    * @param moduleName the module being shadowed
    * @param shadower   the expression shadowing `moduleName`
    * @param location   the location at which the shadowing takes place
    */
  sealed case class SyntheticModule(
    typeName: String,
    moduleName: Name.Qualified,
    override val shadower: IR,
    override val location: Option[IdentifiedLocation]
  ) extends Shadowed {
    override def message(source: (IdentifiedLocation => String)): String =
      s"""Declaration of type $typeName shadows module ${moduleName.name} making it inaccessible via a qualified name."""

    override def diagnosticKeys(): Array[Any] =
      Array(typeName, moduleName, shadower)

  }

  /** Used when the exported type of the module can name conflict with fully qualified names of submodules.
    *
    * @param name          the module name
    * @param tpeName       the name of the exported type leading to conflicts
    * @param firstConflict the name of the module that can be innaccessible because of the name conflict
    * @param shadower      the export statement leading to a conflict
    * @param location      the location of the export statement
    */
  sealed case class TypeInModuleNameConflicts(
    name: String,
    tpeName: String,
    firstConflict: String,
    override val shadower: IR,
    override val location: Option[IdentifiedLocation]
  ) extends Shadowed {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The exported type `$tpeName` in `$name` module will cause name conflict " +
      s"when attempting to use a fully qualified name of the `$firstConflict` module."

    /** The important keys identifying identity of the diagnostic
      */
    override def diagnosticKeys(): Array[Any] =
      Array(name, tpeName, shadower)
  }
}
