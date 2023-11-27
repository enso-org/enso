package org.enso.compiler.core.ir.module.scope.imports

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.randomId
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.{
  DiagnosticStorage,
  Expression,
  IRKind,
  IdentifiedLocation,
  MetadataStorage
}

import java.util.UUID

/** An import of a polyglot class.
  *
  * @param entity      language-specific information on the imported entity
  * @param rename      the name this object should be visible under in the
  *                    importing scope
  * @param location    the source location that the node corresponds to
  * @param passData    the pass metadata associated with this node
  * @param diagnostics compiler diagnostics for this node
  */
sealed case class Polyglot(
  entity: Polyglot.Entity,
  rename: Option[String],
  override val location: Option[IdentifiedLocation],
  override val passData: MetadataStorage      = new MetadataStorage(),
  override val diagnostics: DiagnosticStorage = DiagnosticStorage()
) extends Import
    with IRKind.Primitive {
  var id: UUID @Identifier = randomId

  /** Creates a copy of `this`.
    *
    * @param entity      language-specific information on the imported entity
    * @param rename      the name this object should be visible under in the
    *                    importing scope
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    entity: Polyglot.Entity              = entity,
    rename: Option[String]               = rename,
    location: Option[IdentifiedLocation] = location,
    passData: MetadataStorage            = passData,
    diagnostics: DiagnosticStorage       = diagnostics,
    id: UUID @Identifier                 = id
  ): Polyglot = {
    val res =
      Polyglot(entity, rename, location, passData, diagnostics)
    res.id = id
    res
  }

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Polyglot =
    copy(
      location = if (keepLocations) location else None,
      passData =
        if (keepMetadata) passData.duplicate else new MetadataStorage(),
      diagnostics =
        if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
      id = if (keepIdentifiers) id else randomId
    )

  /** @inheritdoc */
  override def setLocation(
    location: Option[IdentifiedLocation]
  ): Polyglot = copy(location = location)

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Polyglot =
    this

  /** Returns the name this object is visible as from Enso code.
    *
    * @return the visible name of this object
    */
  def getVisibleName: String = rename.getOrElse(entity.getVisibleName)

  /** @inheritdoc */
  override def toString: String =
    s"""
       |Module.Scope.Import.Polyglot(
       |entity = $entity,
       |rename = $rename,
       |location = $location,
       |passData = ${this.showPassData},
       |diagnostics = $diagnostics,
       |id = $id
       |)
       |""".toSingleLine

  /** @inheritdoc */
  override def children: List[IR] = List()

  /** @inheritdoc */
  override def showCode(indent: Int): String = {
    val renamePart = rename.map(name => s"as $name").getOrElse("")
    s"polyglot ${entity.langName} import ${entity.showCode(indent)} $renamePart"
  }
}

object Polyglot {

  /** Represents language-specific polyglot import data. */
  sealed trait Entity {
    val langName: String

    /** Returns the name this object is visible as from Enso code.
      *
      * @return the visible name of this object
      */
    def getVisibleName: String

    def showCode(indent: Int = 0): String
  }

  /** Represents an import of a Java class.
    *
    * @param packageName the name of the package containing the imported
    *                    class
    * @param className   the class name
    */
  case class Java(packageName: String, className: String) extends Entity {
    val langName = "java"

    override def getVisibleName: String = className

    /** Returns the fully qualified Java name of this object.
      *
      * @return the Java-side name of the imported entity
      */
    def getJavaName: String = s"$packageName.$className"

    override def showCode(indent: Int): String =
      s"$packageName.$className"
  }
}
