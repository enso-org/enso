package org.enso.compiler.core.ir

import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.ir.module.scope.{Definition, Export, Import}

import java.util.UUID

/** A representation of a top-level Enso module.
  *
  * Modules may only contain imports and top-level bindings, with no top-level
  * executable code.
  *
  * @param imports the import statements that bring other modules into scope
  * @param exports the export statements for this module
  * @param bindings the top-level bindings for this module
  * @param isPrivate whether or not this module is private (project-private)
  * @param identifiedLocation the source location that the node corresponds to
  * @param passData the pass metadata associated with this node
  */
final case class Module(
  imports: List[Import],
  exports: List[Export],
  bindings: List[Definition],
  isPrivate: Boolean,
  override val identifiedLocation: IdentifiedLocation,
  override val passData: MetadataStorage = new MetadataStorage()
) extends IR
    with IRKind.Primitive
    with LazyDiagnosticStorage
    with LazyId {

  /** Create a module.
    *
    * @param imports the import statements that bring other modules into scope
    * @param exports the export statements for this module
    * @param bindings the top-level bindings for this module
    * @param isPrivate whether or not this module is private (project-private)
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    * @param diagnostics the compiler diagnostics
    */
  def this(
    imports: List[Import],
    exports: List[Export],
    bindings: List[Definition],
    isPrivate: Boolean,
    identifiedLocation: IdentifiedLocation,
    passData: MetadataStorage,
    diagnostics: DiagnosticStorage
  ) = {
    this(imports, exports, bindings, isPrivate, identifiedLocation, passData)
    this.diagnostics = diagnostics
  }

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
    isPrivate: Boolean                   = isPrivate,
    location: Option[IdentifiedLocation] = location,
    passData: MetadataStorage            = passData,
    diagnostics: DiagnosticStorage       = diagnostics,
    id: UUID @Identifier                 = id
  ): Module = {
    if (
      imports != this.imports
      || exports != this.exports
      || bindings != this.bindings
      || isPrivate != this.isPrivate
      || location != this.location
      || passData != this.passData
      || diagnostics != this.diagnostics
      || id != this.id
    ) {
      val res =
        Module(
          imports,
          exports,
          bindings,
          isPrivate,
          location.orNull,
          passData
        )
      res.diagnostics = diagnostics
      res.id          = id
      res
    } else this
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
      passData =
        if (keepMetadata) passData.duplicate else new MetadataStorage(),
      diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
      id          = if (keepIdentifiers) id else null
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

  /** String representation. */
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
