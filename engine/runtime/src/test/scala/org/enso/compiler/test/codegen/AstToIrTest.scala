package org.enso.compiler.test.codegen

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Error.Syntax
import org.enso.compiler.test.CompilerTest
import org.scalatest.Inside

class AstToIrTest extends CompilerTest with Inside {

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
      ir.location shouldBe defined
    }

    "work properly for sides sections" in {
      val ir =
        """
          |(+)
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Section.Sides]
      // TODO[DB] Section.Sides location is not parsed
      //ir.location shouldBe defined
    }

    "work properly for right sections" in {
      val ir =
        """
          |(+ 1)
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Section.Right]
      ir.location shouldBe defined
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

    "support constructor patterns" in {
      val ir =
        """
          |case foo of
          |    Cons a b -> a + b
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Case.Expr]

      ir.branches.head.pattern shouldBe an[IR.Pattern.Constructor]

      val consPat =
        ir.branches.head.pattern.asInstanceOf[IR.Pattern.Constructor]

      consPat.constructor.name shouldEqual "Cons"
      consPat.fields.length shouldEqual 2

      consPat.fields.head shouldBe an[IR.Pattern.Name]
      consPat.fields(1) shouldBe an[IR.Pattern.Name]
    }

    "support catch all patterns" in {
      val ir =
        """
          |case foo of
          |    _ -> 10
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Case.Expr]

      ir.branches.head.pattern shouldBe an[IR.Pattern.Name]
      ir.branches.head.pattern
        .asInstanceOf[IR.Pattern.Name]
        .name shouldBe an[IR.Name.Blank]
    }

    "support named catch all patterns" in {
      val ir =
        """
          |case foo of
          |    a -> a
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Case.Expr]

      ir.branches.head.pattern shouldBe an[IR.Pattern.Name]
      ir.branches.head.pattern
        .asInstanceOf[IR.Pattern.Name]
        .name
        .name shouldEqual "a"
    }

    "support nested patterns" in {
      val ir =
        """
          |case foo of
          |    Cons (Cons a b) _ -> a + b
          |    Cons a Nil -> a
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Case.Expr]

      ir.branches.head.pattern shouldBe an[IR.Pattern.Constructor]
      ir.branches(1).pattern shouldBe an[IR.Pattern.Constructor]

      val branch1 =
        ir.branches.head.pattern.asInstanceOf[IR.Pattern.Constructor]
      branch1.constructor.name shouldEqual "Cons"
      branch1.fields.length shouldEqual 2
      branch1.fields.head shouldBe an[IR.Pattern.Constructor]
      branch1.fields(1) shouldBe an[IR.Pattern.Name]

      val branch1Field1 =
        branch1.fields.head.asInstanceOf[IR.Pattern.Constructor]
      branch1Field1.fields.length shouldEqual 2

      val branch2 =
        ir.branches(1).pattern.asInstanceOf[IR.Pattern.Constructor]

      branch2.constructor.name shouldEqual "Cons"
      branch2.fields.length shouldEqual 2
      branch2.fields.head shouldBe an[IR.Pattern.Name]
      branch2.fields(1) shouldBe an[IR.Pattern.Constructor]
    }

    "support constructor-only patterns" in {
      val ir =
        """
          |case foo of
          |    Nil -> 10
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Case.Expr]

      val pattern = ir.branches.head.pattern
      pattern shouldBe an[IR.Pattern.Constructor]
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

    "support ascribed argument definitions" in {
      val ir =
        """
          |(~x : Type = Type.default) -> x.do_thing
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Lambda]
      val args = ir.asInstanceOf[IR.Function.Lambda].arguments
      args.length shouldEqual 1
      val firstArg = args.head
      firstArg.name.name shouldEqual "x"
      firstArg.ascribedType shouldBe defined
      firstArg.ascribedType.get.asInstanceOf[IR.Name].name shouldEqual "Type"
      firstArg.defaultValue shouldBe defined
      firstArg.suspended shouldBe true
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
      fn.function shouldEqual IR.Name.Literal(
        "negate",
        isMethod = true,
        None
      )

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

    "work for method definitions with ascribed arguments" in {
      val ir =
        """
          |T.f a:b (~c : d = 1) = a + c
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Method.Binding]

      val method =
        ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Method.Binding]

      val args = method.arguments
      args.length shouldEqual 2

      val firstArg = args.head
      firstArg.name.name shouldEqual "a"
      firstArg.ascribedType shouldBe defined
      firstArg.ascribedType.get.asInstanceOf[IR.Name].name shouldEqual "b"
      firstArg.defaultValue should not be defined
      firstArg.suspended shouldBe false

      val secondArg = args(1)
      secondArg.name.name shouldEqual "c"
      secondArg.ascribedType shouldBe defined
      secondArg.ascribedType.get.asInstanceOf[IR.Name].name shouldEqual "d"
      secondArg.defaultValue shouldBe defined
      secondArg.suspended shouldBe true
    }

    "work for method definitions with operator names" in {
      val bindings =
        """
          |My.== : My -> Boolean
          |My.== that = this.a == that.a
          |""".stripMargin.toIrModule.bindings

      val tpIr = bindings.head
      tpIr shouldBe a[IR.Type.Ascription]
      val tp = tpIr.asInstanceOf[IR.Type.Ascription]
      tp.typed shouldBe a[IR.Name.MethodReference]
      val methodRef = tp.typed.asInstanceOf[IR.Name.MethodReference]
      methodRef.typePointer.get.name shouldEqual "My"
      methodRef.methodName.name shouldEqual "=="

      val methodIr = bindings(1)
      methodIr shouldBe a[IR.Module.Scope.Definition.Method.Binding]
      val method =
        methodIr.asInstanceOf[IR.Module.Scope.Definition.Method.Binding]
      method.methodReference.methodName.name shouldEqual "=="
      method.methodReference.typePointer.get.name shouldEqual "My"
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

    "work with ascribed arguments" in {
      val ir =
        """
          |f a:b (~c : d = 1) = a + c
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Binding]
      val func = ir.asInstanceOf[IR.Function.Binding]
      val args = func.arguments
      args.length shouldEqual 2

      val firstArg = args.head
      firstArg.name.name shouldEqual "a"
      firstArg.ascribedType shouldBe defined
      firstArg.ascribedType.get.asInstanceOf[IR.Name].name shouldEqual "b"
      firstArg.defaultValue should not be defined
      firstArg.suspended shouldBe false

      val secondArg = args(1)
      secondArg.name.name shouldEqual "c"
      secondArg.ascribedType shouldBe defined
      secondArg.ascribedType.get.asInstanceOf[IR.Name].name shouldEqual "d"
      secondArg.defaultValue shouldBe defined
      secondArg.suspended shouldBe true
    }
  }

  "AST translation for atom definitions" should {
    "work for atoms with no arguments" in {
      val ir =
        """
          |type My_Type
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Atom]
      val atom = ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Atom]
      atom.name.name shouldEqual "My_Type"
    }

    "work for atoms with arguments" in {
      val ir =
        """
          |type My_Type a b c
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Atom]
      val atom = ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Atom]
      atom.name.name shouldEqual "My_Type"
      val args = atom.arguments
      args.length shouldEqual 3
      args.head.name.name shouldEqual "a"
      args(1).name.name shouldEqual "b"
      args(2).name.name shouldEqual "c"
    }

    "work for atoms with default arguments" in {
      val ir =
        """
          |type My_Type (a = 1)
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Atom]
      val atom = ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Atom]
      atom.name.name shouldEqual "My_Type"
      val args = atom.arguments
      args.length shouldEqual 1
      val firstArg = args.head
      firstArg.name.name shouldEqual "a"
      firstArg.ascribedType should not be defined
      firstArg.defaultValue shouldBe defined
      firstArg.suspended shouldBe false
    }

    "raise an error for atoms with lazy arguments" in {
      val ir =
        """
          |type My_Type ~a
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Error.Syntax]
      val error = ir.bindings.head.asInstanceOf[IR.Error.Syntax]
      error.reason shouldBe an[IR.Error.Syntax.SuspendedArgInAtom.type]
    }

    "work for atoms with ascribed arguments" in {
      val ir =
        """
          |type My_Type a:b (c : d = 1)
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Atom]
      val atom = ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Atom]
      atom.name.name shouldEqual "My_Type"
      val args = atom.arguments
      args.length shouldEqual 2

      val firstArg = args.head
      firstArg.name.name shouldEqual "a"
      firstArg.ascribedType shouldBe defined
      firstArg.defaultValue should not be defined

      val secondArg = args(1)
      secondArg.name.name shouldEqual "c"
      secondArg.ascribedType shouldBe defined
      secondArg.defaultValue shouldBe defined
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

    "disallow type definitions without exploding" in {
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

    "allow defining methods with operator names" in {
      val body =
        """
          |type My
          |    type My a
          |
          |    + : My -> My
          |    + that = My this.a+that.a
          |""".stripMargin.toIrModule.bindings.head
          .asInstanceOf[IR.Module.Scope.Definition.Type]
          .body

      body(1) shouldBe an[IR.Type.Ascription]
      body(2) shouldBe an[IR.Function.Binding]
      val fun = body(2).asInstanceOf[IR.Function.Binding]
      fun.name.name shouldEqual "+"
    }
  }

  "AST translation for documentation comments" should {
    "work at the top level" in {
      val ir =
        """
          |## Some documentation for foo
          |foo a b = a + b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Comment.Documentation]
      ir.asInstanceOf[IR.Comment.Documentation]
        .doc shouldEqual " Some documentation for foo"
    }

    "work within top-level blocks" in {
      val ir =
        """
          |a ->
          |    ## Some docs for b
          |    b = 1
          |    10
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Function.Lambda]

      val comment = ir.body
        .asInstanceOf[IR.Expression.Block]
        .expressions
        .head
      comment shouldBe an[IR.Comment.Documentation]
      comment
        .asInstanceOf[IR.Comment.Documentation]
        .doc shouldEqual " Some docs for b"
    }
  }

  "AST translation for type operators" should {
    "support type ascription" in {
      val ir =
        """
          |a : Type
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support context ascription" in {
      val ir =
        """
          |a in IO
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support error ascription" in {
      val ir =
        """
          |IO ! FileNotFound
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support the subsumption operator" in {
      val ir =
        """
          |IO.Read <: IO
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support the equality operator" in {
      val ir =
        """
          |T ~ Q
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support the concatenation operator" in {
      val ir =
        """
          |a ; b
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support the union operator" in {
      val ir =
        """
          |A | B
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support the intersection operator" in {
      val ir =
        """
          |A & B
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "support the minus operator" in {
      val ir =
        """
          |A \ B
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }
  }

  "AST translation for typeset literals" should {
    "work properly" in {
      val ir =
        """
          |{ x := 0 ; y := 0 }
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Literal.Typeset]
    }
  }

  "AST translation for top-level type signatures" should {
    "work for method signatures" in {
      val ir =
        """
          |MyAtom.foo : Number -> Number -> Number
          |MyAtom.foo = a -> b -> a + b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Type.Ascription]

      val asc = ir.asInstanceOf[IR.Type.Ascription]
      asc.typed shouldBe an[IR.Name.MethodReference]
    }

    "work for module-method signatures" in {
      val ir =
        """
          |foo : Number -> Number
          |foo a = a
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Type.Ascription]
      val asc = ir.asInstanceOf[IR.Type.Ascription]
      asc.typed shouldBe an[IR.Name.MethodReference]
    }

    "work with sugared syntax" in {
      val ir =
        """
          |MyAtom.foo : Number -> Number -> Number
          |MyAtom.foo a b = a + b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Type.Ascription]

      val asc = ir.asInstanceOf[IR.Type.Ascription]
      asc.typed shouldBe an[IR.Name.MethodReference]
    }

    "result in a syntax error if not valid" in {
      val ir =
        """
          |a b : Number -> Number -> Number
          |MyAtom.foo a b = a + b
          |""".stripMargin.toIrModule.bindings.head

      ir shouldBe an[IR.Error.Syntax]
      ir.asInstanceOf[IR.Error.Syntax]
        .reason shouldBe an[Syntax.InvalidStandaloneSignature.type]
    }

    "properly support dotted operators in ascriptions" in {
      val ir =
        """with_output_stream : Vector.Vector -> Any ! File_Error
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Operator.Binary]

      ir.right.value
        .asInstanceOf[IR.Application.Operator.Binary]
        .left
        .value shouldBe an[IR.Name.Qualified]
    }

    "work inside type bodies" in {
      val ir =
        """
          |type MyType
          |    type MyAtom
          |
          |    foo : this -> integer
          |    foo = 0
          |""".stripMargin.toIrModule.bindings.head
          .asInstanceOf[IR.Module.Scope.Definition.Type]

      ir.body.length shouldEqual 3
      ir.body(1) shouldBe an[IR.Type.Ascription]
    }
  }

  "AST translation for expression-level type signatures" should {
    "work in block contexts" in {
      val ir =
        """
          |x : Number
          |x = 10
          |""".stripMargin.toIrExpression.get.asInstanceOf[IR.Expression.Block]

      ir.expressions.head shouldBe an[IR.Application.Operator.Binary]
    }

    "work in expression contexts" in {
      val ir =
        """
          |(a + b) : Number
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Application.Operator.Binary]
    }

    "work properly when used in assignments" in {
      val ir =
        """
          |x = a : Number
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Expression.Binding]

      ir.asInstanceOf[IR.Expression.Binding]
        .expression shouldBe an[IR.Application.Operator.Binary]
    }

    "properly support nested ascriptions" in {
      val ir =
        """
          |x : (a : Type) -> (b : Type -> Type) -> (c : Type)
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Operator.Binary]

      ir.right.value shouldBe an[IR.Function.Lambda]
      ir.right.value
        .asInstanceOf[IR.Function.Lambda]
        .arguments
        .head
        .ascribedType shouldBe defined
    }

    // TODO [AA] Syntax error with `f a ->`

    "properly support dotted operators in ascriptions" in {
      val ir =
        """with_output_stream : Vector.Vector -> Any ! File_Error
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Operator.Binary]

      ir.right.value
        .asInstanceOf[IR.Application.Operator.Binary]
        .left
        .value shouldBe an[IR.Name.Qualified]
    }

    "properly support the `in` context ascription operator" in {
      val ir =
        """
          |x : Number in (Maybe | IO)
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Operator.Binary]

      ir.right.value shouldBe an[IR.Application.Operator.Binary]
      ir.right.value
        .asInstanceOf[IR.Application.Operator.Binary]
        .operator
        .name shouldEqual "in"
    }

    "properly support the `!` error ascription operator" in {
      val ir =
        """
          |x : Number in IO ! OverflowError
          |""".stripMargin.toIrExpression.get
          .asInstanceOf[IR.Application.Operator.Binary]
          .right
          .value

      ir shouldBe an[IR.Application.Operator.Binary]
      ir.asInstanceOf[IR.Application.Operator.Binary]
        .operator
        .name shouldEqual "!"
    }
  }

  "AST translation of top-level annotations" should {
    "support annotations at the top level" in {
      val ir =
        """@My_Annotation
          |type Foo a b
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Name.Annotation]
      ir.bindings(1) shouldBe an[IR.Module.Scope.Definition.Atom]
    }

    "support annotations inside complex type bodies" in {
      val ir =
        """type My_Type
          |  @My_Annotation
          |  type Foo
          |
          |  @My_Annotation
          |  add a = this + a
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Type]
      val complexType =
        ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Type]

      complexType.body.head shouldBe an[IR.Name.Annotation]
      complexType.body(2) shouldBe an[IR.Name.Annotation]
    }
  }

  "AST translation for imports and exports" should {
    "properly support different kinds of imports" in {
      val imports = List(
        "import username.Foo.Bar as Baz",
        "import project.Foo.Bar",
        "from project import all",
        "from Username.Bar.Quux import Baz",
        "from Username.Bar.Test import Baz, Spam",
        "from Username.Bar.Test import Baz, Spam, foo, Bar",
        "from Username.Bar.Test import foo, bar",
        "from username.Foo.Bar import all",
        "from username.Foo.Bar as Eggs import all hiding Spam",
        "from project.Foo.Bar import all hiding Spam, Eggs"
      )
      imports
        .mkString("\n")
        .toIrModule
        .imports
        .map(_.showCode()) shouldEqual imports
    }

    "properly support different kinds of exports" in {
      val exports = List(
        "export Username.Bar as Baz",
        "export project.Bar",
        "from Username.Bar export Baz",
        "from username.Bar export baz, Spam",
        "from project.Bar export all",
        "from username.Bar as Eggs export all hiding Spam",
        "from Username.Bar export all hiding Spam, eggs"
      )
      exports
        .mkString("\n")
        .toIrModule
        .exports
        .map(_.showCode()) shouldEqual exports
    }
  }

  "AST translation of erroneous constructs" should {
    "result in a syntax error when encountering " +
    "unbalanced parentheses in application" in {
      val ir =
        """type MyAtom
          |
          |main =
          |    f = case _ of
          |        Cons (Cons MyAtom Nil) Nil -> 100
          |        _ -> 50
          |    f (Cons (Cons MyAtom Nil) Nil
          |""".stripMargin.toIrModule

      inside(ir.bindings(1)) {
        case binding: IR.Module.Scope.Definition.Method.Binding =>
          inside(binding.body) { case block: IR.Expression.Block =>
            inside(block.returnValue) {
              case application: IR.Application.Prefix =>
                inside(application.arguments.head) {
                  case argument: IR.CallArgument.Specified =>
                    inside(argument.value) { case error: IR.Error.Syntax =>
                      error.reason shouldBe
                      IR.Error.Syntax.AmbiguousExpression
                    }
                }
            }
          }
      }
    }

    "result in a syntax error when encountering " +
    "unbalanced parentheses in a type definition" in {
      val ir =
        """type Maybe
          |    type Nothing
          |    type Just a
          |    (()
          |""".stripMargin.toIrModule
      inside(ir.bindings.head) {
        case definition: IR.Module.Scope.Definition.Type =>
          inside(definition.body(2)) { case error: IR.Error.Syntax =>
            error.reason shouldBe IR.Error.Syntax.UnexpectedDeclarationInType
          }
      }
    }

    "result in a syntax error when encountering " +
    "unbalanced parentheses in a pattern" in {
      val ir =
        """type MyAtom
          |
          |main =
          |    f = case _ of
          |        (Cons (Cons MyAtom Nil) Nil -> 100
          |        _ -> 50
          |""".stripMargin.toIrModule
      inside(ir.bindings(1)) {
        case main: IR.Module.Scope.Definition.Method.Binding =>
          inside(main.body) { case block: IR.Expression.Block =>
            inside(block.returnValue) { case f: IR.Expression.Binding =>
              inside(f.expression) { case app: IR.Application.Prefix =>
                inside(app.arguments(1)) {
                  case arg: IR.CallArgument.Specified =>
                    inside(arg.value) { case argBlock: IR.Expression.Block =>
                      inside(argBlock.expressions.head) {
                        case error: IR.Error.Syntax =>
                          error.reason shouldBe
                          IR.Error.Syntax.AmbiguousExpression
                      }
                    }
                }
              }
            }
          }
      }
    }
  }

  "Bad macro invocations" should {
    "result in syntax errors for imports" in {
      val ir =
        """import
          |
          |main = "foo"
          |""".stripMargin.toIrModule

      ir.bindings.head shouldBe an[IR.Error]
    }

    "result in syntax errors for lambdas with no body" in {
      val ir =
        """a ->
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error]
    }

    "result in syntax errors for lambdas with no args" in {
      val ir =
        """-> a
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error]
    }
  }

  "AST translation of text" should {
    "not need any escapes" in {
      val ir =
        """"Foo bar \ \n baz"
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Literal.Text]
      ir.asInstanceOf[IR.Literal.Text].text shouldEqual "Foo bar \\ \\n baz"
    }
  }
}
