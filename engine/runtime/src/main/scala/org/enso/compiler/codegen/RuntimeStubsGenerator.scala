package org.enso.compiler.codegen

import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.callable.atom.AtomConstructor
import org.enso.interpreter.runtime.data.Type

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
    localBindings.types.foreach { tp =>
      if (tp.builtinType) {
        val builtinType = builtins.getBuiltinType(tp.name)
        if (builtinType == null) {
          throw new CompilerError("Unknown @Builtin_Type " + tp.name)
        }
        if (
          Set(tp.members: _*) != Set(
            builtinType.getConstructors.toIndexedSeq: _*
          )
            .map(_.getName)
        ) {
          throw new CompilerError(
            s"Wrong constructors declared in the builtin ${tp.name}."
          )
        }
        builtinType.getConstructors.foreach(scope.registerConstructor)
        scope.registerType(builtinType.getType)
        builtinType.getType.setShadowDefinitions(scope)
      } else {
        val rtp = new Type(tp.name, scope, builtins.any(), false)
        scope.registerType(rtp)
        tp.members.foreach { name =>
          val constructor = new AtomConstructor(name, scope, rtp)
          scope.registerConstructor(constructor)
        }
      }
    }
  }
}
