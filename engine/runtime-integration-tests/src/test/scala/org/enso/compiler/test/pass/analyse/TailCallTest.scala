package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{Expression, Function, Pattern, Warning}
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.expression.Case
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.TailCall.TailPosition
import org.enso.compiler.pass.analyse.{AliasAnalysis, TailCall}
import org.enso.compiler.pass.{
  IRPass,
  MiniPassFactory,
  PassConfiguration,
  PassGroup,
  PassManager
}
import org.enso.compiler.test.MiniPassTest
import org.enso.compiler.context.LocalScope

class TailCallTest extends MiniPassTest {
  override def testName: String = "Tail call"

  override def miniPassFactory: MiniPassFactory = {
    TailCall
  }

  override def megaPass: IRPass = TailCallMegaPass

  override def megaPassManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  def mkTailContext: InlineContext =
    buildInlineContext(
      localScope       = Some(LocalScope.createEmpty),
      isInTailPosition = Some(true),
      freshNameSupply  = Some(new FreshNameSupply)
    )

  def mkNoTailContext: InlineContext =
    buildInlineContext(
      localScope       = Some(LocalScope.createEmpty),
      isInTailPosition = Some(false),
      freshNameSupply  = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(TailCall).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
  )

  // === The Tests ============================================================

  "Tail call analysis on modules" should {
    val code =
      """
        |Foo.bar = a -> b -> c ->
        |    d = a + b
        |
        |    case c of
        |      Baz a b -> a * b * d
        |      _ -> d
        |
        |type MyAtom a b c
        |
        |Foo.from (that : Bar) = undefined
        |""".stripMargin

    "XX mark methods as tail" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          ir.bindings.head.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        }
      )
    }

    "mark atoms as tail" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          ir.bindings(1).getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        }
      )
    }

    "mark conversions as tail" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          ir.bindings(2).getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        }
      )
    }
  }

  "Tail call analysis on expressions" should {
    val code =
      """
        |x -> y -> z -> x y z
        |""".stripMargin

    "mark the expression as tail if the context requires it" in {
      testInlineCompilation(
        code,
        () => mkTailContext,
        ir => {
          ir.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
        }
      )
    }

    "not mark the expression as tail if the context doesn't require it" in {
      testInlineCompilation(
        code,
        () => mkNoTailContext,
        ir => {
          ir.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
        }
      )
    }

    "mark the value of a tail assignment as non-tail" in {
      testInlineCompilation(
        """
          |foo = a b
          |""".stripMargin,
        () => mkTailContext,
        ir => {
          val binding = ir.asInstanceOf[Expression.Binding]
          binding.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
          binding.expression.getMetadata(TailCall) shouldEqual Some(
            TailPosition.NotTail
          )
        }
      )
    }

    "mark the value of a tail assignment as non-tail (mini pass)" in {
      testInlineCompilation(
        """
          |foo = a b
          |""".stripMargin,
        () => mkTailContext,
        ir => {
          val binding = ir.asInstanceOf[Expression.Binding]
          binding.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
          binding.expression.getMetadata(TailCall) shouldEqual Some(
            TailPosition.NotTail
          )
        }
      )
    }
  }

  "Tail call analysis on functions" should {
    val code =
      """
        |a -> b -> c ->
        |    d = @Tail_Call (a + b)
        |    e = a * c
        |    @Tail_Call (d + e)
        |""".stripMargin

    "mark the last expression of the function as tail" in {
      testInlineCompilation(
        code,
        () => mkTailContext,
        ir => {
          val lambda = ir.asInstanceOf[Function.Lambda]
          val fnBody = lambda.body.asInstanceOf[Expression.Block]
          fnBody.returnValue.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        }
      )
    }

    "mark the other expressions in the function as not tail" in {
      testInlineCompilation(
        code,
        () => mkTailContext,
        ir => {
          val lambda = ir.asInstanceOf[Function.Lambda]
          val fnBody = lambda.body.asInstanceOf[Expression.Block]
          fnBody.expressions.foreach { expr =>
            expr.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
          }
        }
      )
    }

    "warn about misplaced @TailCall annotations" in {
      testInlineCompilation(
        code,
        () => mkTailContext,
        ir => {
          val lambda = ir.asInstanceOf[Function.Lambda]
          val fnBody = lambda.body.asInstanceOf[Expression.Block]
          fnBody
            .expressions(0)
            .asInstanceOf[Expression.Binding]
            .expression
            .diagnosticsList
            .count(_.isInstanceOf[Warning.WrongTco]) shouldEqual 1
          fnBody.returnValue.diagnosticsList
            .count(_.isInstanceOf[Warning.WrongTco]) shouldEqual 0
        }
      )
    }
  }

  "Tail call analysis on local functions" should {
    "handle application involving local functions" in {
      val code = """
                   |adder_two =
                   |    if 0 == 0 then 0 else
                   |        @Tail_Call adder_two
                   |""".stripMargin

      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val fnBody = ir.bindings.head
            .asInstanceOf[definition.Method]
            .body
            .asInstanceOf[Function.Lambda]
            .body
          fnBody
            .asInstanceOf[Expression.Block]
            .returnValue
            .asInstanceOf[Application.Prefix]
            .arguments(2)
            .value
            .asInstanceOf[Expression.Block]
            .returnValue
            .asInstanceOf[Application.Prefix]
            .function
            .diagnosticsList
            .count(_.isInstanceOf[Warning.WrongTco]) shouldEqual 0
        }
      )
    }
  }

  "Tail call analysis on case expressions" should {
    "not mark any portion of the branch functions as tail by default" in {
      val code =
        """
          |Foo.bar = a ->
          |    x = case a of
          |        Lambda fn arg -> fn arg
          |
          |    x
          |""".stripMargin

      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val caseExpr = ir.bindings.head
            .asInstanceOf[definition.Method]
            .body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]
            .expressions
            .head
            .asInstanceOf[Expression.Binding]
            .expression
            .asInstanceOf[Expression.Block]
            .returnValue
            .asInstanceOf[Case.Expr]

          caseExpr.getMetadata(TailCall) shouldEqual Some(
            TailPosition.NotTail
          )
          caseExpr.branches.foreach(branch => {
            val branchExpression =
              branch.expression.asInstanceOf[Application.Prefix]

            branchExpression.getMetadata(TailCall) shouldEqual Some(
              TailPosition.NotTail
            )
          })
        }
      )
    }

    "only mark the branches as tail if the expression is in tail position" in {
      val code =
        """
          |Foo.bar = a ->
          |    case a of
          |      Lambda fn arg -> fn arg
          |""".stripMargin

      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val caseExpr = ir.bindings.head
            .asInstanceOf[definition.Method]
            .body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]
            .returnValue
            .asInstanceOf[Expression.Block]
            .returnValue
            .asInstanceOf[Case.Expr]

          caseExpr.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
          caseExpr.branches.foreach(branch => {
            val branchExpression =
              branch.expression.asInstanceOf[Application.Prefix]

            branchExpression.getMetadata(TailCall) shouldEqual Some(
              TailPosition.Tail
            )
          })
        }
      )
    }

    "mark patters and pattern elements as not tail" in {
      val code =
        """
          |case x of
          |    Cons a b -> a + b
          |""".stripMargin

      testInlineCompilation(
        code,
        () => mkTailContext,
        ir => {
          val caseExpr = ir
            .asInstanceOf[Expression.Block]
            .returnValue
            .asInstanceOf[Case.Expr]
          val caseBranch         = caseExpr.branches.head
          val pattern            = caseBranch.pattern.asInstanceOf[Pattern.Constructor]
          val patternConstructor = pattern.constructor

          pattern.getMetadata(TailCall) shouldEqual Some(
            TailPosition.NotTail
          )
          patternConstructor.getMetadata(TailCall) shouldEqual Some(
            TailPosition.NotTail
          )
          pattern.fields.foreach(f => {
            f.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)

            f.asInstanceOf[Pattern.Name]
              .name
              .getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
          })
        }
      )
    }
  }

  "Tail call analysis on function calls" should {
    "work on function that has tail call return value" in {
      testModuleCompilation(
        """
          |Foo.bar =
          |    IO.println "AAAAA"
          |""".stripMargin,
        () => mkModuleContext,
        ir => {
          val tailCall = ir.bindings.head.asInstanceOf[definition.Method]
          val tailCallBody = tailCall.body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]

          withClue("Mark the arguments as tail") {
            tailCallBody.returnValue
              .asInstanceOf[Application.Prefix]
              .arguments
              .foreach(arg =>
                arg.getMetadata(TailCall) shouldEqual Some(
                  TailPosition.Tail
                )
              )
          }

          withClue(
            "Mark the function call as tail if it is in a tail position"
          ) {
            tailCallBody.returnValue.getMetadata(TailCall) shouldEqual Some(
              TailPosition.Tail
            )
          }
        }
      )
    }

    "work on function that has not-tail call return value" in {
      testModuleCompilation(
        """
          |Foo.bar =
          |    a = b c d
          |    a
          |""".stripMargin,
        () => mkModuleContext,
        ir => {
          val nonTailCall = ir.bindings.head.asInstanceOf[definition.Method]
          val nonTailCallBody = nonTailCall.body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]

          withClue("Mark the arguments as tail") {
            nonTailCallBody
              .expressions(0)
              .asInstanceOf[Expression.Binding]
              .expression
              .asInstanceOf[Application.Prefix]
              .arguments
              .foreach(arg =>
                arg.getMetadata(TailCall) shouldEqual Some(
                  TailPosition.Tail
                )
              )
          }

          withClue(
            "Mark the function call as not tail if it is in a tail position"
          ) {
            nonTailCallBody.expressions.head
              .asInstanceOf[Expression.Binding]
              .expression
              .getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
          }
        }
      )
    }
  }

  "Tail call analysis on blocks" should {
    val code =
      """
        |Foo.bar = a -> b -> c ->
        |    d = a + b
        |    mul = a -> b -> a * b
        |    mul c d
        |""".stripMargin

    "mark the bodies of bound functions as tail properly" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val method = ir.bindings.head.asInstanceOf[definition.Method]
          val block = method.body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]

          block
            .expressions(1)
            .asInstanceOf[Expression.Binding]
            .expression
            .asInstanceOf[Function.Lambda]
            .body
            .getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
        }
      )
    }

    "mark the block expressions as not tail" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val method = ir.bindings.head.asInstanceOf[definition.Method]
          val block = method.body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]

          block.expressions.foreach(expr =>
            expr.getMetadata(TailCall) shouldEqual Some(
              TailPosition.NotTail
            )
          )
        }
      )
    }

    "mark the final expression of the block as tail" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val method = ir.bindings.head.asInstanceOf[definition.Method]
          val block = method.body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]

          block.returnValue.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        }
      )
    }

    "mark the block as tail if it is in a tail position" in {
      testModuleCompilation(
        code,
        () => mkModuleContext,
        ir => {
          val method = ir.bindings.head.asInstanceOf[definition.Method]
          val block = method.body
            .asInstanceOf[Function.Lambda]
            .body
            .asInstanceOf[Expression.Block]

          block.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
        }
      )
    }
  }
}
