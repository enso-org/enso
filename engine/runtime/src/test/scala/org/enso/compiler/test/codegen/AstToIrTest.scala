package org.enso.compiler.test.codegen

import org.enso.compiler.core.IR
import org.enso.compiler.test.CompilerTest

import scala.annotation.unused

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

  "AST translation of unary minus" should {
    "work when parenthesised" in {
      val ir =
        """
          |(-1)
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Literal.Number]
      ir.asInstanceOf[IR.Literal.Number].value shouldEqual "-1"
    }

    "work when not parenthesised" in {
      val ir =
        """
          |-100
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Literal.Number]
      ir.asInstanceOf[IR.Literal.Number].value shouldEqual "-100"
    }

    "work on non-literals" in {
      val ir =
        """
          |-foo
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Prefix]

      val fn = ir.asInstanceOf[IR.Application.Prefix]
      fn.function shouldEqual IR.Name.Literal("negate", None)

      val fooArg = fn.arguments.head.asInstanceOf[IR.CallArgument.Specified]
      fooArg.value shouldBe an[IR.Name.Literal]
      fooArg.value.asInstanceOf[IR.Name.Literal].name shouldEqual "foo"
    }
  }

  "AST translation of function sugar" should {
    "work for function definitions" in {
      val ir =
        """
          |f a b = a + b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Binding]
    }

    "work for method definitions" in {
      val ir =
        """
          |Foo.bar a b = a + b
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Method.Binding]
    }

    "work for method definitions with involved arguments" in {
      val ir =
        """
          |Foo.bar _ (b = 1) ~c = b + c
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Method.Binding]
    }

    "not recognise pattern match bindings" in {
      val ir =
        """
          |F a b = a + b
          |""".stripMargin.toIrExpression.get

      ir should not be an[IR.Function.Binding]
    }

    "work with ignored arguments" in {
      val ir =
        """
          |f _ b = a + b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Binding]
    }

    "work with defaulted arguments" in {
      val ir =
        """
          |f (a = 1) b = a + b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Binding]
    }

    "work with lazy arguments" in {
      val ir =
        """
          |f ~a b = a + b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Binding]
    }
  }

  "AST translation for the inline flow" should {
    "disallow method definitions without exploding" in {
      val ir =
        """
          |Unit.foo a b = a + b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.MethodDefinedInline]
    }

    "disallow type definitions without explocing" in {
      val ir =
        """
          |type MyAtom a b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.TypeDefinedInline]
    }
  }

  "AST translation for type definitions" should {
    "translate atoms properly" in {
      val ir =
        """
          |type MyAtom a b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Module.Scope.Definition.Atom]
    }

    "translate complex type defs properly" in {
      val ir =
        """
          |type Maybe
          |    Nothing
          |    type Just a
          |
          |    is_just = case this of
          |        Just _  -> true
          |        Nothing -> false
          |
          |    fn a b = a + b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Module.Scope.Definition.Type]

      val typeDef = ir.asInstanceOf[IR.Module.Scope.Definition.Type]

      typeDef.name.name shouldEqual "Maybe"
      typeDef.arguments.length shouldEqual 0

      typeDef.body.head shouldBe an[IR.Name.Literal]
      typeDef.body(1) shouldBe an[IR.Module.Scope.Definition.Atom]
      typeDef.body(2) shouldBe an[IR.Expression.Binding]
      typeDef.body(3) shouldBe an[IR.Function.Binding]
    }

    "disallow unexpected expressions in the type body" in {
      val ir =
        """
          |type Maybe
          |    Nothing
          |    type Just a
          |
          |    F a b = _
          |
          |    case foo of
          |        X -> 1
          |
          |    fn a b = a + b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Module.Scope.Definition.Type]

      val typeDef = ir.asInstanceOf[IR.Module.Scope.Definition.Type]

      typeDef.body(2) shouldBe an[IR.Error.Syntax]
      typeDef
        .body(2)
        .asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.UnexpectedDeclarationInType.type]
      typeDef.body(3) shouldBe an[IR.Error.Syntax]
      typeDef
        .body(3)
        .asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.UnexpectedDeclarationInType.type]
    }

    "disallow definitions with 'type' arguments" in {
      val ir =
        """
          |type Maybe a
          |    Nothing
          |    type Just a
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.InvalidTypeDefinition.type]
    }

    "disallow definitions that do not define or include an atom" in {
      val ir =
        """
          |type Maybe
          |     is_just = case this of
          |         Just _  -> True
          |         Nothing -> False
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[IR.Error.Syntax.InterfaceDefinition.type]
    }
  }
}
