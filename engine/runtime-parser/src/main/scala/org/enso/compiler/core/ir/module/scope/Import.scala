package org.enso.compiler.core.ir.module.scope

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{randomId, Identifier, ToStringHelper}
import org.enso.compiler.core.ir.module.Scope
import org.enso.compiler.core.ir.{
  DiagnosticStorage,
  Expression,
  IRKind,
  IdentifiedLocation,
  MetadataStorage,
  Name
}

/** Module-level import statements. */
trait Import extends Scope {

  /** @inheritdoc */
  override def mapExpressions(fn: Expression => Expression): Import

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Import

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Import
}

object Import {

  /** An import statement.
    *
    * @param name        the full path representing the import
    * @param rename      the name this import is visible as
    * @param isAll       is this importing exported names
    * @param onlyNames   exported names selected from the imported module
    * @param hiddenNames exported names hidden from the imported module
    * @param location    the source location that the node corresponds to
    * @param isSynthetic is this import compiler-generated
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Module(
    name: Name.Qualified,
    rename: Option[Name.Literal],
    isAll: Boolean,
    onlyNames: Option[List[Name.Literal]],
    hiddenNames: Option[List[Name.Literal]],
    override val location: Option[IdentifiedLocation],
    isSynthetic: Boolean                        = false,
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Import
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param name        the full `.`-separated path representing the import
      * @param rename      the name this import is visible as
      * @param isAll       is this importing exported names
      * @param onlyNames   exported names selected from the imported module
      * @param hiddenNames exported names hidden from the imported module
      * @param location    the source location that the node corresponds to
      * @param isSynthetic is this import compiler-generated
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name.Qualified                    = name,
      rename: Option[Name.Literal]            = rename,
      isAll: Boolean                          = isAll,
      onlyNames: Option[List[Name.Literal]]   = onlyNames,
      hiddenNames: Option[List[Name.Literal]] = hiddenNames,
      location: Option[IdentifiedLocation]    = location,
      isSynthetic: Boolean                    = isSynthetic,
      passData: MetadataStorage               = passData,
      diagnostics: DiagnosticStorage          = diagnostics,
      id: Identifier                          = id
    ): Module = {
      val res = Module(
        name,
        rename,
        isAll,
        onlyNames,
        hiddenNames,
        location,
        isSynthetic,
        passData,
        diagnostics
      )
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Module =
      copy(
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Module =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: Expression => Expression
    ): Module = this

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Module.Scope.Import.Module(
         |name = $name,
         |rename = $rename,
         |onlyNames = $onlyNames,
         |hiddenNames = $hiddenNames,
         |isAll = $isAll,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] =
      name :: List(
        rename.toList,
        onlyNames.getOrElse(List()),
        hiddenNames.getOrElse(List())
      ).flatten

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val renameCode = rename.map(n => s" as ${n.name}").getOrElse("")
      if (isAll) {
        val onlyPart = onlyNames
          .map(names => " " + names.map(_.name).mkString(", "))
          .getOrElse("")
        val hidingPart = hiddenNames
          .map(names => s" hiding ${names.map(_.name).mkString(", ")}")
          .getOrElse("")
        val all = if (onlyNames.isDefined) "" else " all"
        s"from ${name.name}$renameCode import$onlyPart$all$hidingPart"
      } else {
        s"import ${name.name}$renameCode"
      }
    }

    /** Gets the name of the module visible in this scope, either the
      * original name or the rename.
      *
      * @return the name of this import visible in code
      */
    def getSimpleName: Name = rename.getOrElse(name.parts.last)

    /** Checks whether the import statement allows use of the given
      * exported name.
      *
      * Note that it does not verify if the name is actually exported
      * by the module, only checks if it is syntactically allowed.
      *
      * @param name the name to check
      * @return whether the name could be accessed or not
      */
    def allowsAccess(name: String): Boolean = {
      if (!isAll) return false;
      if (onlyNames.isDefined) {
        onlyNames.get.exists(_.name == name)
      } else if (hiddenNames.isDefined) {
        !hiddenNames.get.exists(_.name == name)
      } else {
        true
      }
    }
  }

}
