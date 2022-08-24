package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import scala.annotation.unused

case object VectorLiterals extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = IRPass.Metadata.Empty

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq(GlobalNames)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq()

  /** The name of the module that contains the Enso stdlib vector definition. */
  val vectorModuleName: String = "Standard.Base.Data.Vector"

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
    val bindings = ir.unsafeGetMetadata(
      BindingAnalysis,
      "no bindings analysis on the current module"
    )
    val vecModule = vectorModule(bindings)
    ir.mapExpressions(doExpression(_, vecModule))
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
  ): IR.Expression = {
    val bindings = inlineContext.module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "no bindings analysis on the current module"
    )
    val vecModule = vectorModule(bindings)
    doExpression(ir, vecModule)
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  private def vectorModule(bindings: BindingsMap): IR.Expression = {
    val modName =
      IR.Name.Literal(name = "Vector", isMethod = false, location = None)

    val modules: List[Module] = bindings.resolvedImports.flatMap { imp =>
      val module = imp.module.unsafeAsModule()
      module :: module.getIr
        .unsafeGetMetadata(
          BindingAnalysis,
          "no binding analysis on an imported module"
        )
        .resolvedExports
        .map { export => export.module.unsafeAsModule() }
    }
    val module = modules.find(_.getName.toString == vectorModuleName)
    module
      .map { module =>
        val withRes = modName.updateMetadata(
          GlobalNames -->> BindingsMap.Resolution(
            BindingsMap
              .ResolvedModule(ModuleReference.Concrete(module))
          )
        )
        withRes
      }
      .getOrElse {
        IR.Error.Resolution(
          modName,
          IR.Error.Resolution.UnresolvedSequenceMacro
        )
      }
  }

  private def doExpression(
    ir: IR.Expression,
    vecModule: IR.Expression
  ): IR.Expression =
    ir.transformExpressions { case seq: IR.Application.Literal.Sequence =>
      val trans = seq.mapExpressions(doExpression(_, vecModule))
      val fromArrayVector = IR.Name.Literal(
        "from_array",
        isMethod = true,
        None
      )
      IR.Application.Prefix(
        fromArrayVector,
        List(
          IR.CallArgument.Specified(None, vecModule.duplicate(), None),
          IR.CallArgument
            .Specified(None, trans.copy(location = None), None)
        ),
        false,
        trans.location
      )
    }
}
