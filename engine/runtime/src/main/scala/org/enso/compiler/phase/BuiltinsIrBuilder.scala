package org.enso.compiler.phase

import org.enso.compiler.core.EnsoParser
import org.enso.compiler.Passes
import org.enso.compiler.context.{
  CompilerContext,
  FreshNameSupply,
  ModuleContext
}
import org.enso.compiler.data.CompilerConfig
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.CompilationStage
import scala.util.Using

/** A phase responsible for initializing the builtins' IR from the provided
  * source.
  */
object BuiltinsIrBuilder {

  /** Builds the IR for the builtins module based on the builtins source file.
    *
    * We guarantee that the builtins file neither imports anything or restricts
    * any exports, and are hence safe to unconditionally run most of the compiler
    * pipeline. We do not want to run codegen, however, as these definitions
    * would conflict with the existing builtins.
    *
    * This is kept as a separate flow as it is independent of the true
    * compilation pipeline
    *
    * @param module the module to build the IR for
    * @param freshNameSupply the compiler's fresh name supply
    * @param passes the compiler's pass manager
    */
  def build(
    context: CompilerContext,
    module: Module,
    freshNameSupply: FreshNameSupply,
    passes: Passes
  ): Unit = {
    val passManager = passes.passManager
    val moduleContext = ModuleContext(
      module          = module,
      freshNameSupply = Some(freshNameSupply),
      compilerConfig  = CompilerConfig(warningsEnabled = false)
    )
    val initialIr = Using(new EnsoParser) { compiler =>
      compiler.compile(module.getSource.getCharacters)
    }.get
    val irAfterModDiscovery = passManager.runPassesOnModule(
      initialIr,
      moduleContext,
      passes.moduleDiscoveryPasses
    )
    context.updateModule(
      module,
      { u =>
        u.ir(irAfterModDiscovery)
        u.compilationStage(CompilationStage.AFTER_PARSING)
      }
    )

    new ExportsResolution().run(List(module))
    val irAfterTypes = passManager.runPassesOnModule(
      irAfterModDiscovery,
      moduleContext,
      passes.globalTypingPasses
    )
    val irAfterCompilation = passManager.runPassesOnModule(
      irAfterTypes,
      moduleContext,
      passes.functionBodyPasses
    )
    context.updateModule(
      module,
      { u =>
        u.ir(irAfterCompilation)
        u.compilationStage(CompilationStage.AFTER_CODEGEN)
      }
    )
  }
}
