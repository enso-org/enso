package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.{Expression, Module, Name}
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.module.scope.Export
import org.enso.compiler.pass.IRPass

/** Desugars shorthand syntaxes in import and export statements.
  */
case object Imports extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = IRPass.Metadata.Empty

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq()

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq()

  val mainModuleName =
    Name.Literal(
      "Main",
      isMethod = false,
      location = None
    )

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir            the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val newImports = ir.imports.map {
      case i: Import.Module =>
        desugarCurrentProjectAlias(i.name, moduleContext)
          .map { newName =>
            val parts = newName.parts
            if (parts.length == 2) {
              i.copy(
                name = newName.copy(parts = parts :+ mainModuleName),
                rename = computeRename(
                  i.rename,
                  i.onlyNames.nonEmpty || i.isAll,
                  parts(1).asInstanceOf[Name.Literal]
                )
              )
            } else { i.copy(name = newName) }
          }
          .getOrElse(
            errors.ImportExport(
              i,
              errors.ImportExport.ProjectKeywordUsedButNotInProject("import")
            )
          )
      case other => other
    }
    val newExports = ir.exports.map {
      case ex: Export.Module =>
        desugarCurrentProjectAlias(ex.name, moduleContext)
          .map { newName =>
            val parts = newName.parts
            if (parts.length == 2) {
              ex.copy(
                name = newName.copy(parts = parts :+ mainModuleName),
                rename = computeRename(
                  ex.rename,
                  ex.onlyNames.nonEmpty || ex.isAll,
                  parts(1).asInstanceOf[Name.Literal]
                )
              )
            } else { ex.copy(name = newName) }
          }
          .getOrElse(
            errors.ImportExport(
              ex,
              errors.ImportExport.ProjectKeywordUsedButNotInProject("export")
            )
          )
      case other => other
    }
    ir.copy(imports = newImports, exports = newExports)
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir

  private def computeRename(
    originalRename: Option[Name.Literal],
    onlyNamesOrAll: Boolean,
    qualName: Name.Literal
  ): Option[Name.Literal] =
    originalRename.orElse(Option.unless(onlyNamesOrAll)(qualName))

  val currentProjectAlias = "project"

  private def desugarCurrentProjectAlias(
    name: Name.Qualified,
    context: ModuleContext
  ): Option[Name.Qualified] = {
    name.parts match {
      case head :: _ if head.name == currentProjectAlias =>
        val pkg = Option(context.getPackage())
        pkg.map { pkg =>
          val namespace = Name.Literal(
            pkg.namespace,
            isMethod = false,
            location = None
          )
          val pkgName =
            Name.Literal(
              pkg.normalizedName,
              isMethod = false,
              location = None
            )
          name.copy(parts = namespace :: pkgName :: name.parts.tail)
        }
      case _ => Some(name)
    }
  }
}
