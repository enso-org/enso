package org.enso.compiler.test.codegen

import org.enso.compiler.core.IR
import org.enso.compiler.test.CompilerTest

class AstToIrTest extends CompilerTest {

  "AST translation of lambda definitions" should {
    "result in a syntax error when defined with multiple arguments" in {
      val ir =
        """x y -> x + y
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error.Syntax]

      ir.asInstanceOf[IR.Error.Syntax].message shouldEqual
      "Syntax is not supported yet: pattern matching function arguments."
    }

    "support standard lambda chaining" in {
      val ir =
        """
          |x -> y -> z -> x
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Lambda]
      ir.asInstanceOf[IR.Function.Lambda].body shouldBe an[IR.Function.Lambda]
      ir.asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body shouldBe an[IR.Function.Lambda]
    }
  }

  "AST translation of operators" should {
    "disallow named arguments to operators" in {
      val ir =
        """
          |(a = 1) + 10
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.NamedArgInOperator.type]
    }
  }

  "AST translation of operator sections" should {
    "work properly for left sections" in {
      val ir =
        """
          |(1 +)
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Section.Left]
    }

    "work properly for sides sections" in {
      val ir =
        """
          |(+)
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Section.Sides]
    }

    "work properly for right sections" in {
      val ir =
        """
          |(+ 1)
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Section.Right]
    }

    "disallow sections with named arguments" in {
      val ir =
        """
          |(+ (left=1))
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.NamedArgInSection.type]
    }
  }

  "AST translation of function applications" should {
    "allow use of blank arguments" in {
      val ir =
        """
          |a b _ d
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Prefix]

      ir.arguments(1) shouldBe an[IR.CallArgument.Specified]
      ir.arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
        .value shouldBe an[IR.Name.Blank]
    }

    "allow use of named blank arguments" in {
      val ir =
        """
          |a b (f = _) c
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Prefix]

      ir.arguments(1) shouldBe an[IR.CallArgument.Specified]
      ir.arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
        .value shouldBe an[IR.Name.Blank]
    }

    "allow method-call syntax on a blank" in {
      val ir =
        """
          |_.foo a b
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Prefix]

      ir.arguments.head shouldBe an[IR.CallArgument.Specified]
      ir.arguments.head
        .asInstanceOf[IR.CallArgument.Specified]
        .value shouldBe an[IR.Name.Blank]
    }

    "allow functions in applications to be blanks" in {
      val ir =
        """
          |_ a b c
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Prefix]

      ir.function shouldBe an[IR.Name.Blank]
    }
  }

  "AST translation of case expressions" should {
    "support a blank scrutinee" in {
      val ir =
        """
          |case _ of
          |    Cons a b -> a + b
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Case.Expr]

      ir.scrutinee shouldBe an[IR.Name.Blank]
    }
  }

  "AST translation of function definitions" should {
    "support ignored arguments" in {
      val ir =
        """
          |_ -> a -> a + 20
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Function.Lambda]

      ir.arguments.head shouldBe an[IR.DefinitionArgument.Specified]
      val blankArg =
        ir.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
      blankArg.name shouldBe an[IR.Name.Blank]
    }

    "support suspended ignored arguments" in {
      val ir =
        """
          |~_ -> a -> a + 20
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Function.Lambda]

      ir.arguments.head shouldBe an[IR.DefinitionArgument.Specified]
      val blankArg =
        ir.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
      blankArg.name shouldBe an[IR.Name.Blank]
    }

    "support ignored arguments with defaults" in {
      val ir =
        """
          |(_ = 10) -> a -> a + 20
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Function.Lambda]

      ir.arguments.head shouldBe an[IR.DefinitionArgument.Specified]
      val blankArg =
        ir.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
      blankArg.name shouldBe an[IR.Name.Blank]
    }
  }

  "AST translation of bindings" should {
    "allow ignored bindings" in {
      val ir =
        """
          |_ = foo a b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Expression.Binding]
      val binding = ir.asInstanceOf[IR.Expression.Binding]

      binding.name shouldBe an[IR.Name.Blank]
    }
  }
}
