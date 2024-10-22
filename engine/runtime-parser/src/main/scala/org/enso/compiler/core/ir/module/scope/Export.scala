package org.enso.compiler.core.ir.module.scope

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.ir.module.Scope
import org.enso.compiler.core.ir.{
  DiagnosticStorage,
  Expression,
  IRKind,
  IdentifiedLocation,
  LazyDiagnosticStorage,
  LazyId,
  MetadataStorage,
  Name
}

import java.util.UUID

/** An export statement */
trait Export extends Scope {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Export

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Export

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Export
}

object Export {

  /** An export statement.
    *
    * @param name the full path representing the export
    * @param rename the name this export is visible as
    * @param onlyNames exported names selected from the exported module
    * @param identifiedLocation the source location that the node corresponds to
    * @param isSynthetic is this export compiler-generated
    * @param passData the pass metadata associated with this node
    */
  sealed case class Module(
    name: Name.Qualified,
    rename: Option[Name.Literal],
    onlyNames: Option[List[Name.Literal]],
    override val identifiedLocation: IdentifiedLocation,
    isSynthetic: Boolean                   = false,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends IR
      with IRKind.Primitive
      with Export
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the full path representing the export
      * @param rename      the name this export is visible as
      * @param onlyNames   exported names selected from the exported module
      * @param location    the source location that the node corresponds to
      * @param isSynthetic is this import compiler-generated
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name.Qualified                  = name,
      rename: Option[Name.Literal]          = rename,
      onlyNames: Option[List[Name.Literal]] = onlyNames,
      isSynthetic: Boolean                  = isSynthetic,
      location: Option[IdentifiedLocation]  = location,
      passData: MetadataStorage             = passData,
      diagnostics: DiagnosticStorage        = diagnostics,
      id: UUID @Identifier                  = id
    ): Module = {
      if (
        name != this.name
        || rename != this.rename
        || onlyNames != this.onlyNames
        || isSynthetic != this.isSynthetic
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Module(
          name,
          rename,
          onlyNames,
          location.orNull,
          isSynthetic,
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
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Module =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Module = this

    /** String representation. */
    override def toString: String =
      s"""
         |Module.Scope.Export.Module(
         |name = $name,
         |rename = $rename,
         |onlyNames = $onlyNames,
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
        onlyNames.getOrElse(List())
      ).flatten

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val renameCode = rename.map(n => s" as ${n.name}").getOrElse("")
      onlyNames match {
        case Some(names) =>
          s"from ${name.name} export ${names.map(_.name).mkString(", ")}$renameCode"
        case None =>
          s"export ${name.name}$renameCode"
      }
    }

    /** Gets the name of the module visible in the importing scope,
      * either the original name or the rename.
      *
      * @return the name of this export visible in code
      */
    def getSimpleName: Name = rename.getOrElse(name.parts.last)

    /** Checks whether the export statement allows use of the given
      * exported name.
      *
      * Note that it does not verify if the name is actually exported
      * by the module, only checks if it is syntactically allowed.
      *
      * @param name the name to check
      * @return whether the name could be accessed or not
      */
    def allowsAccess(name: String): Boolean = {
      if (onlyNames.isDefined) {
        onlyNames.get.exists(_.name.toLowerCase == name.toLowerCase)
      } else {
        true
      }
    }
  }

}
