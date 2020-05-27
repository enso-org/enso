package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.pass.desugar.ComplexType
import org.enso.compiler.pass.resolve.DocumentationComments
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest

class ComplexTypeTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: List[IRPass] = passes.getPrecursors(ComplexType).get

  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(precursorPasses, passConfig)

  /** Adds an extension method to run complex type desugaring on an
    * [[IR.Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: IR.Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any complex type definitions desugared
      */
    def desugar(implicit moduleContext: ModuleContext): IR.Module = {
      ComplexType.runModule(ir, moduleContext)
    }
  }

  /** Creates a defaulted module context.
    *
    * @return a defaulted module context
    */
  def mkModuleContext: ModuleContext = {
    ModuleContext()
  }

  // === The Tests ============================================================

  "Valid complex types" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |type Maybe
        |    Nothing
        |    type Just a
        |
        |    is_just = case this of
        |        Nothing -> false
        |        Just _  -> true
        |
        |    f a = this + a
        |""".stripMargin.preprocessModule.desugar

    "have their atoms desugared to top-level atoms" in {
      val ir =
        """
          |type MyType
          |    type Foo
          |    type Bar
          |""".stripMargin.preprocessModule.desugar

      exactly(2, ir.bindings) shouldBe an[Definition.Atom]
      ir.bindings.head.asInstanceOf[Definition.Atom].name.name shouldEqual "Foo"
      ir.bindings(1).asInstanceOf[Definition.Atom].name.name shouldEqual "Bar"
    }

    "have their methods desugared to methods on included atoms" in {
      ir.bindings(1) shouldBe an[Definition.Method.Binding]
      val justIsJust = ir.bindings(1).asInstanceOf[Definition.Method.Binding]
      justIsJust.methodName.name shouldEqual "is_just"
      justIsJust.typeName.name shouldEqual "Nothing"

      ir.bindings(3) shouldBe an[Definition.Method.Binding]
      val justF = ir.bindings(3).asInstanceOf[Definition.Method.Binding]
      justF.methodName.name shouldEqual "f"
      justF.typeName.name shouldEqual "Nothing"
    }

    "have their methods desugared to methods on the defined atoms" in {
      ir.bindings(2) shouldBe an[Definition.Method.Binding]
      val justIsJust = ir.bindings(2).asInstanceOf[Definition.Method.Binding]
      justIsJust.methodName.name shouldEqual "is_just"
      justIsJust.typeName.name shouldEqual "Just"

      ir.bindings(4) shouldBe an[Definition.Method.Binding]
      val justF = ir.bindings(4).asInstanceOf[Definition.Method.Binding]
      justF.methodName.name shouldEqual "f"
      justF.typeName.name shouldEqual "Just"
    }
  }
}
