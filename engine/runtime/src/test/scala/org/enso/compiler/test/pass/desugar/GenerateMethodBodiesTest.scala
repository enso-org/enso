package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.pass.desugar.GenerateMethodBodies
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class GenerateMethodBodiesTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  implicit val ctx: ModuleContext = buildModuleContext()

  val precursorPasses: PassGroup =
    passes.getPrecursors(GenerateMethodBodies).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run method and method body generation on an
    * [[IR.Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: IR.Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any method bodies desugared
      */
    def desugar(implicit moduleContext: ModuleContext): IR.Module = {
      GenerateMethodBodies.runModule(ir, moduleContext)
    }
  }

  // === The Tests ============================================================

  "Methods with functions as bodies" should {
    val ir =
      """
        |Unit.method = a -> b -> c -> a + b + c
        |""".stripMargin.preprocessModule
    val irMethod = ir.bindings.head.asInstanceOf[Method]

    val irResult       = ir.desugar
    val irResultMethod = irResult.bindings.head.asInstanceOf[Method]

    "have the `this` argument prepended to the argument list" in {
      val resultArgs =
        irResultMethod.body.asInstanceOf[IR.Function.Lambda].arguments

      resultArgs.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .name shouldEqual IR.Name.This(None)

      resultArgs.tail shouldEqual irMethod.body
        .asInstanceOf[IR.Function.Lambda]
        .arguments
    }

    "have the body of the function remain untouched" in {
      val inputBody  = irMethod.body.asInstanceOf[IR.Function.Lambda].body
      val resultBody = irResultMethod.body.asInstanceOf[IR.Function.Lambda].body

      inputBody shouldEqual resultBody
    }
  }

  "Methods with expressions as bodies" should {
    val ir =
      """
        |Unit.method = 1
        |""".stripMargin.preprocessModule
    val irMethod = ir.bindings.head.asInstanceOf[Method]

    val irResult       = ir.desugar
    val irResultMethod = irResult.bindings.head.asInstanceOf[Method]

    "have the expression converted into a function" in {
      irResultMethod.body shouldBe an[IR.Function.Lambda]
    }

    "have the resultant function take the `this` argument" in {
      val bodyArgs =
        irResultMethod.body.asInstanceOf[IR.Function.Lambda].arguments

      bodyArgs.length shouldEqual 1
      bodyArgs.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .name shouldEqual IR.Name.This(None)
    }

    "have the body of the function be equivalent to the expression" in {
      irResultMethod.body
        .asInstanceOf[IR.Function.Lambda]
        .body shouldEqual irMethod.body
    }

    "have the body function's location equivalent to the original body" in {
      irMethod.body.location shouldEqual irResultMethod.body.location
    }
  }

  "Methods that redefine `this`" should {
    val ir =
      """
        |Unit.method = this -> this + 1
        |""".stripMargin.preprocessModule.desugar

    val method =
      ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Method]

    "have their bodies replaced by an error" in {
      method.body shouldBe an[IR.Error.Redefined.ThisArg]
    }
  }
}
