package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.pass.resolve.OverloadsResolution
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class OverloadsResolutionTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(OverloadsResolution).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method for resolution on the input IR.
    *
    * @param ir the IR to desugar
    */
  implicit class ResolveModule(ir: IR.Module) {

    /** Runs section desugaring on [[ir]].
      *
      * @param moduleContext the module context in which the resolution takes
      *                      place
      * @return [[ir]], with all sections desugared
      */
    def resolve(implicit moduleContext: ModuleContext): IR.Module = {
      OverloadsResolution.runModule(ir, moduleContext)
    }
  }

  /** Makes a module context.
    *
    * @return a new module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Method overload resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val atomName   = "Unit"
    val methodName = "foo"

    val ir =
      s"""
         |$atomName.$methodName = x -> x
         |$atomName.$methodName = x -> y -> x + y
         |$atomName.$methodName = 10
         |""".stripMargin.preprocessModule.resolve

    "detect overloads within a given module" in {
      exactly(2, ir.bindings) shouldBe an[IR.Error.Redefined.Method]
    }

    "replace all overloads by an error node" in {
      ir.bindings(1) shouldBe an[IR.Error.Redefined.Method]
      ir.bindings(2) shouldBe an[IR.Error.Redefined.Method]
      val redef1 = ir.bindings(1).asInstanceOf[IR.Error.Redefined.Method]
      val redef2 = ir.bindings(2).asInstanceOf[IR.Error.Redefined.Method]

      redef1.atomName.get.name shouldEqual atomName
      redef2.atomName.get.name shouldEqual atomName

      redef1.methodName.name shouldEqual methodName
      redef2.methodName.name shouldEqual methodName
    }
  }

  "Conversion overload resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    "allow overloads on the source type" in {
      val ir =
        """Unit.from (that : Integer) = undefined
          |Unit.from (that : Boolean) = undefined
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 2
      ir.bindings.head shouldBe a[Method.Conversion]
      ir.bindings(1) shouldBe a[Method.Conversion]
    }

    "raise an error if there are multiple definitions with the same source type" in {
      val ir =
        """Unit.from (that : Integer) = undefined
          |Unit.from (that : Boolean) = undefined
          |Unit.from (that : Boolean) = undefined
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 3
      ir.bindings.head shouldBe a[Method.Conversion]
      ir.bindings(1) shouldBe a[Method.Conversion]
      ir.bindings(2) shouldBe an[IR.Error.Redefined.Conversion]
    }
  }

  "Atom overload resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val atomName = "MyAtom"

    val ir =
      s"""
         |type $atomName a b c
         |type $atomName a b
         |type $atomName a
         |""".stripMargin.preprocessModule.resolve

    "detect overloads within a given module" in {
      exactly(2, ir.bindings) shouldBe an[IR.Error.Redefined.Type]
    }

    "replace all overloads by an error node" in {
      ir.bindings(1) shouldBe an[IR.Error.Redefined.Type]
      ir.bindings(1)
        .asInstanceOf[IR.Error.Redefined.Type]
        .typeName
        .name shouldEqual atomName
      ir.bindings(2) shouldBe an[IR.Error.Redefined.Type]
      ir.bindings(2)
        .asInstanceOf[IR.Error.Redefined.Type]
        .typeName
        .name shouldEqual atomName
    }
  }

}
