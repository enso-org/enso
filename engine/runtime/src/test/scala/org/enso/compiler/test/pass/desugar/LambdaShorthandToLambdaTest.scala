package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Literal,
  Name
}
import org.enso.compiler.core.ir.expression.{Application, Case}
import org.enso.compiler.pass.desugar.LambdaShorthandToLambda
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class LambdaShorthandToLambdaTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(LambdaShorthandToLambda).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method for running desugaring on the input IR.
    *
    * @param ir the IR to desugar
    */
  implicit class DesugarExpression(ir: Expression) {

    /** Runs lambda shorthand desugaring on [[ir]].
      *
      * @param inlineContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all lambda shorthand desugared
      */
    def desugar(implicit inlineContext: InlineContext): Expression = {
      LambdaShorthandToLambda.runExpression(ir, inlineContext)
    }
  }

  /** Makes an inline context.
    *
    * @return a new inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Desugaring of underscore arguments" should {
    "Work for simple applications with underscore args" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |foo a _ b _
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn = ir.asInstanceOf[Function.Lambda]
      val irFnArgName =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      irFn.body shouldBe an[Function.Lambda]
      val irFnNested = irFn.body.asInstanceOf[Function.Lambda]
      val irFnNestedArgName = irFnNested.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name

      irFnNested.body shouldBe an[Application.Prefix]
      val body = irFn.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Prefix]

      val arg2Name = body
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val arg4Name = body
        .arguments(3)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

      irFnArgName.name shouldEqual arg2Name.name
      irFnNestedArgName.name shouldEqual arg4Name.name
    }

    "Work for named applications of underscore args" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |foo (a = _) b _
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn = ir.asInstanceOf[Function.Lambda]
      val irFnArgName =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      irFn.body shouldBe an[Function.Lambda]
      val irFnNested = irFn.body.asInstanceOf[Function.Lambda]
      val irFnNestedArgName =
        irFnNested.arguments.head
          .asInstanceOf[DefinitionArgument.Specified]
          .name

      irFnNested.body shouldBe an[Application.Prefix]
      val app = irFnNested.body.asInstanceOf[Application.Prefix]
      val arg1Name =
        app.arguments.head
          .asInstanceOf[CallArgument.Specified]
          .value
          .asInstanceOf[Name.Literal]
      val arg3Name =
        app
          .arguments(2)
          .asInstanceOf[CallArgument.Specified]
          .value
          .asInstanceOf[Name.Literal]

      irFnArgName.name shouldEqual arg1Name.name
      irFnNestedArgName.name shouldEqual arg3Name.name
    }

    "Work if the function in an application is an underscore arg" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |_ a b
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn = ir.asInstanceOf[Function.Lambda]
      val irFnArgName =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      irFn.body shouldBe an[Application.Prefix]
      val app = irFn.body.asInstanceOf[Application.Prefix]

      val fnName = app.function.asInstanceOf[Name.Literal]

      irFnArgName.name shouldEqual fnName.name
    }

    "Work with mixfix functions" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |if _ then a
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn = ir.asInstanceOf[Function.Lambda]
      val irFnArgName =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      irFn.body shouldBe an[Application.Prefix]
      val app = irFn.body.asInstanceOf[Application.Prefix]
      val arg1Name = app.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

      irFnArgName.name shouldEqual arg1Name.name
    }

    "Work for an underscore scrutinee in a case expression" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |case _ of
          |    Nil -> 0
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined

      val irLam = ir.asInstanceOf[Function.Lambda]
      irLam.arguments.length shouldEqual 1

      val lamArgName =
        irLam.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      val lamBody = irLam.body.asInstanceOf[Case.Expr]

      lamBody.scrutinee shouldBe an[Name.Literal]
      lamBody.scrutinee
        .asInstanceOf[Name.Literal]
        .name shouldEqual lamArgName.name
    }

    "work correctly for operators" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(10 + _)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn = ir.asInstanceOf[Function.Lambda]

      val argName =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      val body     = irFn.body.asInstanceOf[Application.Prefix]
      val rightArg = body.arguments(1).value.asInstanceOf[Name.Literal]

      argName.name shouldEqual rightArg.name
    }

    "work correctly for left operator sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(_ +)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val underscoreFn      = ir.asInstanceOf[Function.Lambda]
      val underscoreArgName = underscoreFn.arguments.head.name

      val rightArgLambda = underscoreFn.body.asInstanceOf[Function.Lambda]
      rightArgLambda.arguments.length shouldEqual 1
      val rightArgLambdaArgName = rightArgLambda.arguments.head.name

      val body = rightArgLambda.body.asInstanceOf[Application.Prefix]
      body.arguments.length shouldEqual 2

      val leftCallArgName =
        body.arguments.head.value.asInstanceOf[Name.Literal]
      val rightCallArgName =
        body.arguments(1).value.asInstanceOf[Name.Literal]

      underscoreArgName.name shouldEqual leftCallArgName.name
      rightArgLambdaArgName.name shouldEqual rightCallArgName.name
    }

    "work correctly for centre operator sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(_ + _)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn     = ir.asInstanceOf[Function.Lambda]
      val arg1Name = irFn.arguments.head.name

      val irFn2    = irFn.body.asInstanceOf[Function.Lambda]
      val arg2Name = irFn2.arguments.head.name

      val app = irFn2.body.asInstanceOf[Application.Prefix]
      app.arguments.length shouldEqual 2

      val leftArg  = app.arguments.head.value.asInstanceOf[Name.Literal]
      val rightArg = app.arguments(1).value.asInstanceOf[Name.Literal]

      arg1Name.name shouldEqual leftArg.name
      arg2Name.name shouldEqual rightArg.name
    }

    "work correctly for right operator sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+ _)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn         = ir.asInstanceOf[Function.Lambda]
      val rightArgName = irFn.arguments.head.name

      irFn.body shouldBe an[Function.Lambda]
      val irFn2       = irFn.body.asInstanceOf[Function.Lambda]
      val leftArgName = irFn2.arguments.head.name

      irFn2.body shouldBe an[Application.Prefix]
      val app = irFn2.body.asInstanceOf[Application.Prefix]

      app.arguments.length shouldEqual 2

      val appLeftName  = app.arguments.head.value.asInstanceOf[Name.Literal]
      val appRightName = app.arguments(1).value.asInstanceOf[Name.Literal]

      leftArgName.name shouldEqual appLeftName.name
      rightArgName.name shouldEqual appRightName.name
    }

    "work for vector literals" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |[1, _, (3 + 4), _]
          |""".stripMargin.preprocessExpression.get.desugar

      val fun1 = ir.asInstanceOf[Function.Lambda]
      val fun2 = fun1.body.asInstanceOf[Function.Lambda]
      val vec  = fun2.body.asInstanceOf[Application.Sequence]

      fun1.arguments(0).name shouldEqual vec.items(1)
      fun2.arguments(0).name shouldEqual vec.items(3)
      vec.items(0) shouldBe an[Literal.Number]
      vec.items(2) shouldBe an[Application.Prefix]
    }
  }

  "Nested underscore arguments" should {
    "work for applications" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |a _ (fn _ c)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.asInstanceOf[Function.Lambda].body shouldBe an[Application.Prefix]
      val irBody = ir
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Prefix]

      irBody
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value shouldBe an[Function.Lambda]
      val lamArg = irBody
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Function.Lambda]
      val lamArgArgName =
        lamArg.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      lamArg.body shouldBe an[Application.Prefix]
      val lamArgBody = lamArg.body.asInstanceOf[Application.Prefix]
      val lamArgBodyArg1Name = lamArgBody.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

      lamArgArgName.name shouldEqual lamArgBodyArg1Name.name
    }

    "work in named applications" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |a _ (fn (t = _) c)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.asInstanceOf[Function.Lambda].body shouldBe an[Application.Prefix]
      val irBody = ir
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Prefix]

      irBody
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value shouldBe an[Function.Lambda]
      val lamArg = irBody
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Function.Lambda]
      val lamArgArgName =
        lamArg.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      lamArg.body shouldBe an[Application.Prefix]
      val lamArgBody = lamArg.body.asInstanceOf[Application.Prefix]
      val lamArgBodyArg1Name = lamArgBody.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

      lamArgArgName.name shouldEqual lamArgBodyArg1Name.name
    }

    "work in function argument defaults" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |a -> (b = f _ 1) -> f a
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      val bArgFn = ir
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
      val bArg1 =
        bArgFn.arguments.head.asInstanceOf[DefinitionArgument.Specified]

      bArg1.defaultValue shouldBe defined
      bArg1.defaultValue.get shouldBe an[Function.Lambda]
      val default = bArg1.defaultValue.get.asInstanceOf[Function.Lambda]
      val defaultArgName = default.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name

      default.body shouldBe an[Application.Prefix]
      val defBody = default.body.asInstanceOf[Application.Prefix]
      val defBodyArg1Name = defBody.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

      defaultArgName.name shouldEqual defBodyArg1Name.name
    }

    "work for case expressions" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |case _ of
          |    Nil -> f _ b
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      val nilBranch = ir
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Case.Expr]
        .branches
        .head
        .asInstanceOf[Case.Branch]

      nilBranch.expression shouldBe an[Function.Lambda]
      val nilBody = nilBranch.expression.asInstanceOf[Function.Lambda]

      val nilBodyArgName =
        nilBody.arguments.head
          .asInstanceOf[DefinitionArgument.Specified]
          .name

      nilBody.body shouldBe an[Application.Prefix]
      val nilBodyBody = nilBody.body.asInstanceOf[Application.Prefix]
      val nilBodyBodyArg1Name = nilBodyBody.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

      nilBodyArgName.name shouldEqual nilBodyBodyArg1Name.name
    }
  }

  "A single lambda shorthand" should {
    "translate to `id` in general" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x = _
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Expression.Binding]
      val expr = ir.asInstanceOf[Expression.Binding].expression

      expr shouldBe an[Function.Lambda]
    }

    "translate to an `id` in argument defaults" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(x = _) -> x
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      val irFn = ir.asInstanceOf[Function.Lambda]

      irFn.arguments.head shouldBe an[DefinitionArgument.Specified]
      val arg =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified]

      arg.defaultValue shouldBe defined
      arg.defaultValue.get shouldBe an[Function.Lambda]
    }
  }

  "Lambda shorthand in nested functions" should {
    "correctly translate the section-function in an application" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """(_ + 5) 5
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Application.Prefix]
      val app = ir.asInstanceOf[Application.Prefix]
      app.function shouldBe an[Function.Lambda]
      val lam = app.function.asInstanceOf[Function.Lambda]
      lam.arguments.length shouldEqual 1
      val lamArg1Name = lam.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name
      val lamBody = lam.body.asInstanceOf[Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val appArg1Name = lamBody.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
        .name
      lamArg1Name shouldEqual appArg1Name
    }

    "correctly translate the function in an application" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """(f _ _ b) b
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Application.Prefix]
      val irFn = ir.asInstanceOf[Application.Prefix].function
      irFn shouldBe an[Function.Lambda]
      val firstLam = irFn.asInstanceOf[Function.Lambda]
      firstLam.arguments.length shouldEqual 1
      val firstLamArgName = firstLam.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name
      val secondLam = firstLam.body.asInstanceOf[Function.Lambda]
      val secondLamArgName = secondLam.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name
      val app = secondLam.body.asInstanceOf[Application.Prefix]
      app.arguments.length shouldEqual 3
      val appArg1Name = app.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name]
        .name
      val appArg2Name = app
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name]
        .name
      firstLamArgName shouldEqual appArg1Name
      secondLamArgName shouldEqual appArg2Name
    }
  }
}
