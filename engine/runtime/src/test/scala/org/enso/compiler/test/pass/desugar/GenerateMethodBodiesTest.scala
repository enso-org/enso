package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.ConstantsNames
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Function,
  Module,
  Name,
  Warning
}
import org.enso.compiler.core.ir.expression.Operator
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.pass.desugar.{FunctionBinding, GenerateMethodBodies}
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class GenerateMethodBodiesTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  implicit val ctx: ModuleContext = buildModuleContext()

  val precursorPasses: PassGroup =
    passes.getPrecursors(GenerateMethodBodies).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run method and method body generation on an
    * [[Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any method bodies desugared
      */
    def desugar(implicit moduleContext: ModuleContext): Module = {
      GenerateMethodBodies.runModule(ir, moduleContext)
    }
  }

  // === The Tests ============================================================

  "Methods with functions as bodies" should {
    val ir =
      """
        |Unit.method = a -> b -> c -> a + b + c
        |""".stripMargin.preprocessModule
    val irMethod = ir.bindings.head.asInstanceOf[definition.Method]

    val irResult       = ir.desugar
    val irResultMethod = irResult.bindings.head.asInstanceOf[definition.Method]

    "still have the `self` argument prepended to the argument list" in {
      val resultArgs =
        irResultMethod.body.asInstanceOf[Function.Lambda].arguments

      val firstArg :: restArgs = resultArgs
      val self = firstArg
        .asInstanceOf[DefinitionArgument.Specified]
        .name
      self shouldBe a[Name.Self]
      self.asInstanceOf[Name.Self].synthetic shouldBe true

      restArgs shouldEqual irMethod.body
        .asInstanceOf[Function.Lambda]
        .arguments
    }

    "have the body of the function remain untouched" in {
      val inputBody  = irMethod.body.asInstanceOf[Function.Lambda].body
      val resultBody = irResultMethod.body.asInstanceOf[Function.Lambda].body

      inputBody shouldEqual resultBody
    }
  }

  "Methods with expressions as bodies" should {
    val ir =
      """
        |Unit.method = 1
        |""".stripMargin.preprocessModule
    val irMethod = ir.bindings.head.asInstanceOf[definition.Method]

    val irResult       = ir.desugar
    val irResultMethod = irResult.bindings.head.asInstanceOf[definition.Method]

    "have the expression converted into a function" in {
      irResultMethod.body shouldBe an[Function.Lambda]
    }

    "have the resultant function take the `self` argument" in { // TODO old semantics
      val bodyArgs =
        irResultMethod.body.asInstanceOf[Function.Lambda].arguments

      bodyArgs.length shouldEqual 1
    }

    "have the body of the function be equivalent to the expression" in {
      irResultMethod.body
        .asInstanceOf[Function.Lambda]
        .body shouldEqual irMethod.body
    }

    "have the body function's location equivalent to the original body" in {
      irMethod.body.location shouldEqual irResultMethod.body.location
    }
  }

  "Methods with explicit self and expressions as bodies" should {
    val ir =
      """
        |Unit.method self = 1
        |""".stripMargin.preprocessModule
    val irMethod = ir.bindings.head.asInstanceOf[definition.Method]

    val irResult       = ir.desugar
    val irResultMethod = irResult.bindings.head.asInstanceOf[definition.Method]

    "have the expression converted into a function" in {
      irResultMethod.body shouldBe an[Function.Lambda]
    }

    "have the resultant function take the `self` argument" in { // TODO old semantics
      val bodyArgs =
        irResultMethod.body.asInstanceOf[Function.Lambda].arguments

      bodyArgs.length shouldEqual 1
      val self = bodyArgs.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
      self shouldBe a[Name.Self]
      self.asInstanceOf[Name.Self].synthetic shouldBe false
    }

    "have the body of the function be equivalent to the expression" in {
      irResultMethod.body
        .asInstanceOf[Function.Lambda]
        .body shouldEqual irMethod.body.asInstanceOf[Function.Lambda].body
    }

    "have the body function's location equivalent to the original body" in {
      irMethod.body.location shouldEqual irResultMethod.body.location
    }
  }

  "Methods that redefine `self`" should {
    val ir =
      """
        |Unit.method = self -> self + 1
        |Unit.foo = a -> self -> self + 1 + a
        |Unit.bar self = self + 1
        |Unit.baz a self = self + 1 + a
        |qux self a self = a
        |""".stripMargin.preprocessModule

    val irMethod =
      ir.bindings.head.asInstanceOf[definition.Method]
    val irMethodSelfArg =
      irMethod.body.asInstanceOf[Function.Lambda].arguments.head
    val irFoo =
      ir.bindings(1).asInstanceOf[definition.Method]
    val irFooFirstArg =
      irFoo.body.asInstanceOf[Function.Lambda].arguments.head
    val irBar =
      ir.bindings(2).asInstanceOf[definition.Method]
    val irBarFirstArg =
      irBar.body.asInstanceOf[Function.Lambda].arguments.head
    val irBaz =
      ir.bindings(3).asInstanceOf[definition.Method]
    val irBazSndArg = irBaz.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Function.Lambda]
      .arguments
      .head

    val irResult       = ir.desugar
    val irResultMethod = irResult.bindings.head.asInstanceOf[definition.Method]
    val irResultFoo    = irResult.bindings(1).asInstanceOf[definition.Method]
    val irResultBar    = irResult.bindings(2).asInstanceOf[definition.Method]
    val irResultBaz    = irResult.bindings(3).asInstanceOf[definition.Method]
    val irResultQux    = irResult.bindings(4).asInstanceOf[definition.Method]

    "not generate an auxiliary self parameter" in {
      val resultArgs = irResultMethod.body
        .asInstanceOf[Function.Lambda]
        .arguments

      resultArgs.size shouldEqual 1
      val selfArg = resultArgs.head.name
      selfArg shouldEqual Name.Self(
        location  = irMethodSelfArg.name.location,
        synthetic = false
      )
    }

    "generate a warning about self parameter not being in the first position" in {
      val resultLambda = irResultFoo.body
        .asInstanceOf[Function.Lambda]
      val resultArgs = resultLambda.arguments

      resultArgs.size shouldEqual 1
      val selfArg = resultArgs.head.name
      selfArg should not be an[Name.Self]
      selfArg shouldEqual irFooFirstArg.name
    }

    "not generate an auxiliary self parameter for the already present one" in {
      val resultLambda = irResultBar.body
        .asInstanceOf[Function.Lambda]
      val resultArgs = resultLambda.arguments

      resultArgs.size shouldEqual 1
      val selfArg = resultArgs.head.name
      selfArg shouldBe an[Name.Self]
      resultLambda.body shouldBe an[Operator.Binary]
      selfArg shouldEqual Name.Self(location = irBarFirstArg.location)
    }

    "not generate an auxiliary self parameter for the already present one but in a wrong position" in {
      val resultLambda = irResultBaz.body
        .asInstanceOf[Function.Lambda]
      val resultArgs = resultLambda.arguments

      resultArgs.size shouldEqual 1
      val firstArg = resultArgs.head.name
      firstArg should not be an[Name.Self]

      val bodyLambda = resultLambda.body.asInstanceOf[Function.Lambda]
      bodyLambda.arguments.size shouldEqual 1
      val selfArg = bodyLambda.arguments.head.name
      selfArg shouldEqual Name.Self(location = irBazSndArg.location)
      resultLambda.diagnostics.collect { case w: Warning =>
        w
      }.head shouldBe an[Warning.WrongSelfParameterPos]
    }

    "return an error when redefining `self` parameter" in {
      irResultQux.body shouldBe an[errors.Redefined.SelfArg]
    }
  }

  "Methods that provide an explicit `self` parameter" should {
    val ir =
      """
        |type Foo
        |
        |    type Foo a
        |
        |    add self b = self.a + b
        |
        |    sum b self = self.a + b
        |""".stripMargin.preprocessModule.desugar

    val irMethodAdd =
      ir.bindings(2).asInstanceOf[definition.Method]
    val irMethodAddSelfArg =
      irMethodAdd.body.asInstanceOf[Function.Lambda].arguments

    val irResult          = ir.desugar
    val irResultMethodAdd = irResult.bindings(2).asInstanceOf[definition.Method]

    val irResultMethodSum = irResult.bindings(3).asInstanceOf[definition.Method]

    "not add new argument" in {
      val resultLambda = irResultMethodAdd.body
        .asInstanceOf[Function.Lambda]
      val resultArgs = resultLambda.arguments

      resultArgs.size shouldEqual 1
      val selfArg = resultArgs.head.name
      selfArg shouldEqual Name.Self(location =
        irMethodAddSelfArg.head.name.location
      )
      resultLambda.diagnostics.collect { case w: Warning =>
        w
      } shouldBe empty
    }

    "generate a warning when the parameter is not first" in {
      val resultLambda = irResultMethodSum.body
        .asInstanceOf[Function.Lambda]
      val resultArgs = resultLambda.arguments

      resultArgs.size shouldEqual 1
      val selfArg = resultArgs(0).name
      selfArg should not be an[Name.Self]
      resultLambda.diagnostics.collect { case w: Warning =>
        w
      }.head shouldBe an[Warning.WrongSelfParameterPos]

      val nestedLmabda = resultLambda.body.asInstanceOf[Function.Lambda]
      nestedLmabda.arguments.size shouldEqual 1
      nestedLmabda.arguments(0).name shouldBe an[Name.Self]
    }
  }

  "Conversion method definitions" should {
    val from = FunctionBinding.conversionMethodName

    "have the `self` argument added" in { // TODO old semantics
      val ir =
        s"""My_Type.$from (that : Other) = that.a
           |""".stripMargin.preprocessModule.desugar

      val conversion =
        ir.bindings.head.asInstanceOf[definition.Method]
      conversion.body shouldBe an[Function.Lambda]
      val body = conversion.body.asInstanceOf[Function.Lambda]
      body.arguments.length shouldEqual 2
      body.arguments.head.name shouldBe a[Name.Self]
    }

    // FIXME: This should probably be prohibited
    "have the `self` argument unchanged when defined explicitly" in {
      val ir =
        s"""My_Type.$from (self : My_Type) (that : Other) = that.a
           |""".stripMargin.preprocessModule.desugar

      val conversion =
        ir.bindings.head.asInstanceOf[definition.Method]
      conversion.body shouldBe an[Function.Lambda]
      val body = conversion.body.asInstanceOf[Function.Lambda]
      body.arguments.length shouldEqual 1
      body.arguments.head.name shouldBe an[Name.Self]
      val nestedBody = body.body.asInstanceOf[Function.Lambda]
      nestedBody.arguments.length shouldEqual 1
      nestedBody.arguments.head.name shouldBe an[Name.Literal]
      nestedBody.arguments.head.name.name shouldEqual ConstantsNames.THAT_ARGUMENT
    }

    "have report a warning when defining `self` at a wrong position" in {
      val ir =
        s"""My_Type.$from (that : Other) self = that
           |""".stripMargin.preprocessModule.desugar

      val conversion =
        ir.bindings.head.asInstanceOf[definition.Method]
      conversion.body shouldBe an[Function.Lambda]
      val body = conversion.body.asInstanceOf[Function.Lambda]
      body.arguments.length shouldEqual 1
      body.arguments.head.name shouldBe an[Name.Literal]
      body.arguments.head.name.name shouldBe ConstantsNames.THAT_ARGUMENT

      conversion.body.diagnostics.collect { case w: Warning =>
        w
      }.head shouldBe an[Warning.WrongSelfParameterPos]
    }

    "have report a warning when defining default `self` at a wrong position" in {
      val ir =
        s"""My_Type.$from (that : Other) self=1 = that.a
           |""".stripMargin.preprocessModule.desugar

      val conversion =
        ir.bindings.head.asInstanceOf[definition.Method]
      conversion.body shouldBe an[Function.Lambda]
      val body = conversion.body.asInstanceOf[Function.Lambda]
      body.arguments.length shouldEqual 1
      body.arguments.head.name shouldBe an[Name.Literal]
      body.arguments.head.name.name shouldBe ConstantsNames.THAT_ARGUMENT

      conversion.body.diagnostics.collect { case w: Warning =>
        w
      }.head shouldBe an[Warning.WrongSelfParameterPos]
    }

  }
}
