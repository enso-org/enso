package org.enso.compiler.codegen

import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.callable.atom.AtomConstructor

/**
  * Generates stubs of runtime representations of atom constructors, to allow
  * [[IrToTruffle the code generator]] to refer to constructors that are not
  * fully generated yet.
  */
class RuntimeStubsGenerator() {

  /**
    * Runs the stage on the given module.
    *
    * @param module the module to generate stubs in.
    */
  def run(module: Module): Unit = {
    val ir    = module.getIr
    val scope = module.getScope
    val localBindings = ir.unsafeGetMetadata(
      BindingAnalysis,
      "Non-parsed module used in stubs generator"
    )
    localBindings.types.foreach { tp =>
      val constructor = new AtomConstructor(tp.name, scope)
      scope.registerConstructor(constructor)
    }
  }
}
