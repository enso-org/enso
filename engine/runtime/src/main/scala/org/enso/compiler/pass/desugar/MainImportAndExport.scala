package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** Desugars imports and exports mentioning only the project name to refer
  * to the `Main` module of the mentioned project instead.
  */
case object MainImportAndExport extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = IRPass.Metadata.Empty

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq()

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq()

  private val mainModuleName =
    IR.Name.Literal("Main", isReferent = true, location = None)

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
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val newImports = ir.imports.map {
      case i: IR.Module.Scope.Import.Module =>
        val parts = i.name.parts
        if (parts.length == 1) {
          i.copy(
            name = i.name.copy(parts = parts :+ mainModuleName),
            rename =
              computeRename(i.rename, parts.head.asInstanceOf[IR.Name.Literal])
          )
        } else { i }
      case other => other
    }
    val newExports = ir.exports.map { ex =>
      val parts = ex.name.parts
      if (parts.length == 1) {
        ex.copy(
          name = ex.name.copy(parts = parts :+ mainModuleName),
          rename =
            computeRename(ex.rename, parts.head.asInstanceOf[IR.Name.Literal])
        )
      } else {
        ex
      }
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
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  private def computeRename(
    originalRename: Option[IR.Name.Literal],
    qualName: IR.Name.Literal
  ): Some[IR.Name.Literal] = Some(originalRename.getOrElse(qualName))
}
