package org.enso.compiler.pass.lint

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.core.ir.expression.warnings
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.Export
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.ComplexType

/** Generates warnings about potential name conflicts between types and synthetic modules
  */
case object ModuleNameConflicts extends IRPass {

  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override lazy val precursorPasses: Seq[IRPass] = Seq(ComplexType)

  /** The passes that are invalidated by running this pass. */
  override lazy val invalidatedPasses: Seq[IRPass] = Seq()

  /** Lints a module
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    if (moduleContext.compilerConfig.warningsEnabled) {
      val syntheticExports = ir.exports.flatMap {
        case mod @ Export.Module(
              _,
              _,
              false,
              None,
              None,
              None,
              true,
              _,
              _
            ) =>
          Some(mod)
        case mod: Export.Module if moduleContext.isSynthetic() =>
          Some(mod)
        case _ =>
          None
      }
      ir.copy(
        bindings = ir.bindings.map(lintBinding(_, syntheticExports))
      )
    } else {
      ir
    }
  }

  /** Acts as an identity function to conform with the interface.
    *
    * @param ir the Enso IR to process.
    * @param inlineContext a context object that contains the information
    *                      needed for inline evaluation.
    * @return unchanged ir.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression =
    ir

  // === Pass Internals =======================================================

  /** Lints a binding
    *
    * @param binding the binding to lint
    * @param syntheticExports a list of compiler-added module exports in this module
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def lintBinding(
    binding: Definition,
    syntheticExports: List[Export.Module]
  ): Definition = {
    val exports = syntheticExports.map(e => (e.name.parts.last.name, e)).toMap

    binding match {
      case cons: Definition.Type if exports.contains(cons.name.name) =>
        val atomName = cons.name.name
        val `export` = exports(atomName)
        binding.addDiagnostic(
          warnings.Shadowed
            .SyntheticModule(atomName, `export`.name, `export`, cons.location)
        )
      case _ =>
        binding
    }
  }
}
