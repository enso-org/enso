package org.enso.compiler.codegen

import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.callable.atom.AtomConstructor

/** Generates stubs of runtime representations of atom constructors, to allow
  * [[IrToTruffle the code generator]] to refer to constructors that are not
  * fully generated yet.
  */
class RuntimeStubsGenerator(builtins: Builtins) {

  /** Runs the stage on the given module.
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
    localBindings.constructors.foreach { tp =>
      if (tp.builtinType) {
        val builtinType = builtins.getBuiltinType(tp.name)
        if (builtinType == null) {
          throw new CompilerError("Unknown @BuiltinType " + tp.name)
        }
        scope.registerConstructor(builtinType)
        builtinType.setShadowDefinitions(scope)
      } else {
        val constructor = new AtomConstructor(tp.name, scope)
        scope.registerConstructor(constructor)
      }
    }
  }
}
