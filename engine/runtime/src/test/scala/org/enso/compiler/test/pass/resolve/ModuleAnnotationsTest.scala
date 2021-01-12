package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.IR
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.resolve.ModuleAnnotations
import org.enso.compiler.test.CompilerTest

class ModuleAnnotationsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: PassGroup = passes.getPrecursors(ModuleAnnotations).get

  val passConfiguration: PassConfiguration = PassConfiguration();

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method for running module-level annotations resolution
    * on the input IR.
    *
    * @param ir the IR to resolve
    */
  implicit class ResolveModule(ir: IR.Module) {

    /** Runs annotation resolution on an [[IR.Module]].
      *
      * @param moduleContext the module context in which the resolution takes
      *                      place
      * @return [[ir]], with all module-level annotations resolved
      */
    def resolve(implicit moduleContext: ModuleContext): IR.Module = {
      ModuleAnnotations.runModule(ir, moduleContext)
    }
  }

  /** Makes a module context.
    *
    * @return a fresh module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext()
  }

  // === The Tests ============================================================

  "Annotation desugaring at the top level" should {
    "associate annotations with atom definitions" in {}

    "associate annotations with complex type definitions" in {}

    "associate annotations with method definitions" in {}

    "not associate annotations with comments" in {}
  }

  "Annotation desugaring in complex types" should {
    "associate annotations with atom definitions" in {}

    "associate annotations with method definitions" in {}

    "not associate annotations with comments" in {}
  }
}
