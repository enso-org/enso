package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.expression.errors.Syntax
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.pass.analyse.PrivateConstructorAnalysis
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class PrivateModifierTest extends CompilerTest {

  // === Utilities ============================================================

  val passes = new Passes(defaultConfig)

  /** The passes that need to be run before the alias analysis pass. */
  val precursorPasses: PassGroup = passes.getPrecursors(PrivateConstructorAnalysis.INSTANCE).get

  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run private constructor analysis on an [[Module]].
   *
   * @param ir the module to run private constructor analysis on
   */
  implicit class AnalyseModule(ir: Module) {

    /** Runs alias analysis on a module.
     *
     * @return [[ir]], with attached aliasing information
     */
    def analyse: Module = {
      PrivateConstructorAnalysis.INSTANCE.runModule(
        ir,
        buildModuleContext(passConfiguration = Some(passConfig))
      )
    }
  }

  /** Creates a defaulted module context.
   *
   * @return a defaulted module context
   */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Private constructor analysis" should {
    implicit val ctx: ModuleContext = mkModuleContext

    "should accept a single private constructor" in {
      val ir =
        """
          |type My_Type
          |    private Value a b c
          |""".stripMargin.preprocessModule.analyse
      ir.bindings.size shouldBe(1)
      ir.bindings.head.isInstanceOf[Definition.Type] shouldBe(true)
      val tp = ir.bindings.head.asInstanceOf[Definition.Type]
      tp.members.size shouldBe(1)
      tp.members.head
    }

    "should reject mix of public and private constructors in a type" in {
      val ir =
        """
          |type My_Type
          |    private Priv_Cons a b
          |    Pub_Cons c d
          |""".stripMargin.preprocessModule.analyse
      val errors = ir.preorder().collect { case err: Syntax => err}
      errors.size shouldBe(1)
      errors.head.reason shouldBe(Syntax.InconsistentConstructorVisibility)
    }

    "should accept more private constructors in a type" in {
      val ir =
        """
          |type My_Type
          |    Pub_Cons_1 a b
          |    Pub_Cons_2 c d
          |""".stripMargin.preprocessModule.analyse
      val errors = ir.preorder().collect { case err: Syntax => err}
      errors.isEmpty shouldBe(true)
    }
  }
}
