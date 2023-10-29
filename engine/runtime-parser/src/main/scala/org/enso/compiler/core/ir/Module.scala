package org.enso.compiler.core.ir

import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.randomId
import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.ir.module.scope.{Definition, Export, Import}

import java.util.UUID

/** A representation of a top-level Enso module.
  *
  * Modules may only contain imports and top-level bindings, with no top-level
  * executable code.
  *
  * @param imports     the import statements that bring other modules into scope
  * @param exports     the export statements for this module
  * @param bindings    the top-level bindings for this module
  * @param isPrivate    whether or not this module is private (project-private)
  * @param location    the source location that the node corresponds to
  * @param passData    the pass metadata associated with this node
  * @param diagnostics compiler diagnostics for this node
  */
@SerialVersionUID(
  8160L // Use BindingsMap
)       // prevents reading broken caches, see PR-3692 for details
sealed case class Module(
  imports: List[Import],
  exports: List[Export],
  bindings: List[Definition],
  isPrivate: Boolean,
  location: Option[IdentifiedLocation],
  passData: MetadataStorage      = MetadataStorage(),
  diagnostics: DiagnosticStorage = DiagnosticStorage()
) extends IR
    with IRKind.Primitive {
  var id: UUID @Identifier = randomId

  /** Creates a copy of `this`.
    *
    * @param imports     the import statements that bring other modules into scope
    * @param exports     the export statements for this module
    * @param bindings    the top-level bindings for this module
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    imports: List[Import]                = imports,
    exports: List[Export]                = exports,
    bindings: List[Definition]           = bindings,
    location: Option[IdentifiedLocation] = location,
    passData: MetadataStorage            = passData,
    diagnostics: DiagnosticStorage       = diagnostics,
    id: UUID @Identifier                 = id
  ): Module = {
    val res =
      Module(
        imports,
        exports,
        bindings,
        isPrivate,
        location,
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
      imports = imports.map(
        _.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        )
      ),
      bindings = bindings.map(
        _.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        )
      ),
      location = if (keepLocations) location else None,
      passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
      diagnostics =
        if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
      id = if (keepIdentifiers) id else randomId
    )

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Module =
    copy(location = location)

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Module = {
    copy(
      imports  = imports.map(_.mapExpressions(fn)),
      exports  = exports.map(_.mapExpressions(fn)),
      bindings = bindings.map(_.mapExpressions(fn))
    )
  }

  /** @inheritdoc */
  override def children: List[IR] = imports ++ exports ++ bindings

  /** @inheritdoc */
  override def toString: String =
    s"""
       |Module(
       |imports = $imports,
       |exports = $exports,
       |bindings = $bindings,
       |location = $location,
       |passData = ${this.showPassData},
       |diagnostics = $diagnostics,
       |id = $id
       |)
       |""".toSingleLine

  /** @inheritdoc */
  override def showCode(indent: Int): String = {
    val importsString = imports.map(_.showCode(indent)).mkString("\n")
    val exportsString = exports.map(_.showCode(indent)).mkString("\n")
    val defsString    = bindings.map(_.showCode(indent)).mkString("\n\n")

    List(importsString, exportsString, defsString).mkString("\n\n")
  }
}
