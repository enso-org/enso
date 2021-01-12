package org.enso.compiler.phase

import org.enso.compiler.Passes
import org.enso.compiler.codegen.AstToIr
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.Module.CompilationStage
import org.enso.syntax.text.Parser

import scala.annotation.unused

/** A phase responsible for initializing the builtins' IR from the provided
  * source.
  */
object BuiltinsIrBuilder {

  // TODO [AA] Run the compilation pipeline as far as needed.
  def build(
    @unused module: Module,
    @unused freshNameSupply: FreshNameSupply,
    @unused passes: Passes
  ): Unit = {
    val passManager = passes.passManager
    module.ensureScopeExists()
    module.getScope.reset()
    val moduleContext = ModuleContext(
      module          = module,
      freshNameSupply = Some(freshNameSupply)
    )
    val parsedAst = Parser().runWithIds(module.getSource.getCharacters.toString)

    val initialIr = AstToIr.translate(parsedAst)
    val irAfterModDiscovery = passManager.runPassesOnModule(
      initialIr,
      moduleContext,
      passes.moduleDiscoveryPasses
    )
    val irAfterCompilation = passManager.runPassesOnModule(
      irAfterModDiscovery,
      moduleContext,
      passes.functionBodyPasses
    )
    module.unsafeSetIr(irAfterCompilation)
    module.unsafeSetCompilationStage(CompilationStage.AFTER_CODEGEN)
  }
}
