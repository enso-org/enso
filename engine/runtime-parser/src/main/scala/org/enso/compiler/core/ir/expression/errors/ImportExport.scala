package org.enso.compiler.core.ir
package expression
package errors

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID
import scala.annotation.unused

/** An erroneous import or export statement.
  *
  * @param ir          the original statement
  * @param reason      the reason it's erroneous
  * @param passData    the pass data
  * @param diagnostics the attached diagnostics
  */
sealed case class ImportExport(
  ir: IR,
  reason: ImportExport.Reason,
  override val passData: MetadataStorage      = new MetadataStorage(),
  override val diagnostics: DiagnosticStorage = DiagnosticStorage()
) extends Error
    with Diagnostic.Kind.Interactive
    with org.enso.compiler.core.ir.module.scope.Import
    with org.enso.compiler.core.ir.module.scope.Export
    with IRKind.Primitive
    with LazyId {

  /** Creates a copy of `this`.
    *
    * @param ir          the original IR
    * @param reason      the cause of this error
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    ir: IR                         = ir,
    reason: ImportExport.Reason    = reason,
    passData: MetadataStorage      = passData,
    diagnostics: DiagnosticStorage = diagnostics,
    id: UUID @Identifier           = id
  ): ImportExport = {
    val res = ImportExport(ir, reason, passData, diagnostics)
    res.id = id
    res
  }

  /** @inheritdoc */
  override def duplicate(
    @unused keepLocations: Boolean = true,
    keepMetadata: Boolean          = true,
    keepDiagnostics: Boolean       = true,
    keepIdentifiers: Boolean       = false
  ): ImportExport =
    copy(
      passData =
        if (keepMetadata) passData.duplicate else new MetadataStorage(),
      diagnostics =
        if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
      id = if (keepIdentifiers) id else null
    )

  /** @inheritdoc */
  override def setLocation(
    location: Option[IdentifiedLocation]
  ): ImportExport =
    this

  /** @inheritdoc */
  override val location: Option[IdentifiedLocation] = ir.location

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): ImportExport =
    this

  /** @inheritdoc */
  override def toString: String =
    s"""
       |Error.ImportExport(
       |ir = $ir,
       |reason = $reason,
       |location = $location,
       |passData = ${this.showPassData},
       |diagnostics = $diagnostics,
       |id = $id
       |)
       |""".toSingleLine

  /** @inheritdoc */
  override def children: List[IR] = List(ir)

  /** @inheritdoc */
  override def message(source: (IdentifiedLocation => String)): String =
    reason.message(source)

  override def diagnosticKeys(): Array[Any] = Array(reason)

  /** @inheritdoc */
  override def showCode(indent: Int): String = "Import_Export_Error"
}

object ImportExport {

  /** A reason for a statement being erroneous.
    */
  sealed trait Reason {

    /** @param source Location of the original import/export IR.
      * @return A human-readable description of the error.
      */
    def message(source: (IdentifiedLocation => String)): String
  }

  /** Used when the `project` keyword is used in an impossible position.
    *
    * @param statementType the type of statement being affected, see the
    *                      implementation for its grammatical use.
    */
  case class ProjectKeywordUsedButNotInProject(statementType: String)
      extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The `project` keyword was used in an $statementType statement," +
      " but the module does not belong to a project."
  }

  /** Used when an import statement triggers loading of a package that could
    * not be loaded.
    *
    * @param name the module name.
    */
  case class PackageCouldNotBeLoaded(name: String, reason: String)
      extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Package containing the module $name" +
      s" could not be loaded: $reason"
  }

  /** Used when an import statement refers to a module that does not exist.
    *
    * @param name the module name.
    */
  case class ModuleDoesNotExist(name: String) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The module $name does not exist."
  }

  case class TypeDoesNotExist(
    typeName: String,
    moduleName: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The type $typeName does not exist in module $moduleName"
  }

  case class SymbolDoesNotExist(
    symbolName: String,
    moduleOrTypeName: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"The symbol $symbolName (module, type, or constructor) does not exist in $moduleOrTypeName."
  }

  case class NoSuchConstructor(
    typeName: String,
    constructorName: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"No such constructor ${constructorName} in type $typeName"
  }

  case class ExportSymbolsFromPrivateModule(
    moduleName: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Cannot export any symbol from module '$moduleName': The module is private"
  }

  case class ExportPrivateModule(
    moduleName: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Cannot export private module '$moduleName'"
  }

  case class ImportPrivateModule(
    moduleName: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Cannot import private module '$moduleName'"
  }

  case class SubmoduleVisibilityMismatch(
    moduleName: String,
    submoduleName: String,
    moduleVisibility: String,
    submoduleVisibility: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Cannot export submodule '$submoduleName' of module '$moduleName': " +
      s"the submodule is $submoduleVisibility, but the module is $moduleVisibility"
  }

  /** Represents an ambiguous import resolution error, where the same symbol is imported more than once refereing
    * to different objects. The objects are represented by their physical path in the project.
    *
    * @param originalImport     the original import statement.
    * @param originalSymbolPath the original symbol path.
    * @param symbolName         the symbol name that is ambiguous.
    * @param symbolPath         the symbol path that is different than [[originalSymbolPath]].
    */
  case class AmbiguousImport(
    originalImport: module.scope.Import,
    originalSymbolPath: String,
    symbolName: String,
    symbolPath: String
  ) extends Reason {
    override def message(source: (IdentifiedLocation => String)): String = {
      val originalImportRepr =
        originalImport.location match {
          case Some(location) => source(location)
          case None           => originalImport.showCode()
        }
      s"Symbol '$symbolName' resolved ambiguously to '$symbolPath' in the import Statement. " +
      s"The symbol was first resolved to '$originalSymbolPath' in the import statement '$originalImportRepr'."
    }

  }
}
