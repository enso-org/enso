package org.enso.compiler.test.context

import org.enso.compiler.context.SuggestionBuilder
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames, Suggestion}
import org.enso.polyglot.data.Tree
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class SuggestionBuilderTest extends AnyWordSpecLike with Matchers {
  private val ctx = new InterpreterContext()
  private val langCtx = ctx.ctx
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  implicit private class PreprocessModule(code: String) {

    def preprocessModule(name: QualifiedName): IR.Module = {
      val module = new runtime.Module(
        name,
        null,
        code.stripMargin.linesIterator.mkString("\n")
      )
      langCtx.getCompiler.run(module)
      module.getIr
    }

    def preprocessModule: IR.Module =
      preprocessModule(Module)

  }

  private val Module = QualifiedName(List("Unnamed"), "Test")
  private val ModuleNode = Tree.Node(
    Suggestion.Module(
      module        = Module.toString,
      documentation = None
    ),
    Vector()
  )
  private val moduleDoc = "Module doc"
  private val DoccedModuleNode = Tree.Node(
    Suggestion.Module(
      module        = Module.toString,
      documentation = Some(" " + moduleDoc)
    ),
    Vector()
  )

  @annotation.nowarn
  def endOfLine(line: Int, character: Int): Suggestion.Position =
    Suggestion.Position(line + 1, 0)

  "SuggestionBuilder" should {

    "build method without explicit arguments" in {

      val code   = """foo = 42"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with documentation" in {

      val code =
        """## Module doc
          |
          |## The foo
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = Some(" The foo"),
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with annotations" in {

      val code =
        """@a foo
          |@b bar
          |foo a b = a + b""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq("a", "b")
            ),
            Vector()
          )
        )
      )
    }

    "build method with type and documentation" in {

      val code =
        """## Module doc
          |
          |## The foo
          |foo : Number
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = "Number",
              isStatic      = true,
              documentation = Some(" The foo"),
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with a qualified type" in {

      val code =
        """
          |foo : Foo.Bar
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = "Foo.Bar",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with an argument" in {

      val code =
        """import Standard.Base.Data.Text.Text
          |import Standard.Base.Data.Numbers.Number
          |
          |foo : Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Standard.Base.Data.Text.Text",
                  false,
                  false,
                  None
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Standard.Base.Data.Numbers.Number",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with a type containing higher kinds" in {

      val code =
        """
          |foo : Either (Vector Number) Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Either (Vector Number) Text",
                  false,
                  false,
                  None
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Number",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with a type containing qualified higher kinds" in {
      pending // issue #1711

      val code =
        """
          |foo : Foo.Bar Baz
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = "Foo.Bar Baz",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with complex body" in {

      val code =
        """foo a b =
          |    x : Number
          |    x = a + 1
          |    y = b - 2
          |    x * y""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  "Unnamed.Test",
                  "x",
                  "Number",
                  Suggestion
                    .Scope(
                      Suggestion.Position(0, 9),
                      Suggestion.Position(4, 9)
                    ),
                  None
                ),
                Vector()
              ),
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  "Unnamed.Test",
                  "y",
                  SuggestionBuilder.Any,
                  Suggestion
                    .Scope(
                      Suggestion.Position(0, 9),
                      Suggestion.Position(4, 9)
                    ),
                  None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build method with default arguments" in {

      val code   = """foo (a = 0) = a + 1"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, true, Some("0"))
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with explicit self type" in {

      val code =
        """type MyType
          |
          |MyType.bar self a b = a + b
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyType",
              params        = Seq(),
              returnType    = "Unnamed.Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "bar",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyType", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "not build method with undefined self type" in {

      val code =
        """MyAtom.bar a b = a + b"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(Vector(ModuleNode))
    }

    "build method with associated type signature" in {

      val code =
        """type MyAtom a
          |
          |## My bar
          |MyAtom.bar : Number -> Number -> Number
          |MyAtom.bar self a b = a + b
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyAtom",
              params = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyAtom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "bar",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument("a", "Number", false, false, None),
                Suggestion.Argument("b", "Number", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyAtom",
              returnType    = "Number",
              isStatic      = false,
              documentation = Some(" My bar"),
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with function type signature" in {

      val code =
        """type MyAtom
          |
          |MyAtom.apply : (Number -> Number) -> Number
          |MyAtom.apply self f = f self
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyAtom",
              params        = Seq(),
              returnType    = "Unnamed.Test.MyAtom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "apply",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument("f", "Number -> Number", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyAtom",
              returnType    = "Number",
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with union type signature" in {

      val code =
        """type My_Atom
          |    Variant_1
          |    Variant_2
          |
          |type Other_Atom
          |
          |Other_Atom.apply : (Number | Other_Atom | My_Atom) -> Number
          |Other_Atom.apply self f = f self
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "My_Atom",
              params        = Seq(),
              returnType    = "Unnamed.Test.My_Atom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Variant_1",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.My_Atom",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Variant_2",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.My_Atom",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Other_Atom",
              params        = Seq(),
              returnType    = "Unnamed.Test.Other_Atom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "apply",
              arguments = Seq(
                Suggestion
                  .Argument(
                    "self",
                    "Unnamed.Test.Other_Atom",
                    false,
                    false,
                    None
                  ),
                Suggestion.Argument(
                  "f",
                  "Number | Unnamed.Test.Other_Atom | Unnamed.Test.My_Atom",
                  false,
                  false,
                  None,
                  Some(
                    Seq(
                      "Number",
                      "Unnamed.Test.Other_Atom",
                      "Unnamed.Test.My_Atom.Variant_1",
                      "Unnamed.Test.My_Atom.Variant_2"
                    )
                  )
                )
              ),
              selfType      = "Unnamed.Test.Other_Atom",
              returnType    = "Number",
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build argument tag values" in {

      val code =
        """import Standard.Base.Data.Text.Text
          |import Standard.Base.Data.Boolean.Boolean
          |
          |type Value
          |    A
          |    B
          |
          |type Auto
          |
          |foo : Text | Boolean | Value | Auto -> Any
          |foo a = a
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Value",
              params        = Seq(),
              returnType    = "Unnamed.Test.Value",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Auto",
              params        = Seq(),
              returnType    = "Unnamed.Test.Auto",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Standard.Base.Data.Text.Text | Standard.Base.Data.Boolean.Boolean | Unnamed.Test.Value | Unnamed.Test.Auto",
                  false,
                  false,
                  None,
                  Some(
                    Seq(
                      "Standard.Base.Data.Boolean.Boolean.True",
                      "Standard.Base.Data.Boolean.Boolean.False",
                      "Unnamed.Test.Value.A",
                      "Unnamed.Test.Value.B",
                      "Unnamed.Test.Auto"
                    )
                  )
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Any",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with lazy arguments" in {

      val code =
        """foo ~a = a + 1"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, true, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with resolved type signature" in {

      val code =
        """type A
          |
          |foo : A -> A
          |foo a = a + 1""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              params        = Seq(),
              returnType    = "Unnamed.Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  Some(Seq("Unnamed.Test.A"))
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Unnamed.Test.A",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build conversion method for simple type" in {
      pending

      val code =
        """type MyAtom a
          |
          |## My conversion
          |MyAtom.from : Number -> MyAtom
          |MyAtom.from a = MyAtom a
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              params = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Conversion(
              externalId = None,
              module     = "Unnamed.Test",
              arguments = Seq(
                Suggestion.Argument("a", "Number", false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              selfType      = "Number",
              documentation = Some(" My conversion")
            ),
            Vector()
          )
        )
      )
    }

    "build conversion method for complex type" in {
      pending
      val code =
        """type MyMaybe
          |    Some a
          |    None
          |
          |type New
          |    Newtype x
          |
          |## My conversion method
          |New.from : MyMaybe -> New
          |New.from opt = case opt of
          |    Some a -> Newtype a
          |    None -> Newtype 0
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyMaybe",
              params        = Seq(),
              returnType    = "Unnamed.Test.MyMaybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Some",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyMaybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyMaybe", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyMaybe",
              returnType    = SuggestionBuilder.Any,
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "None",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.None",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "New",
              params        = Seq(),
              returnType    = "Unnamed.Test.New",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Newtype",
              arguments = Seq(
                Suggestion
                  .Argument("x", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.New",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "x",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.New", false, false, None)
              ),
              selfType      = "Unnamed.Test.New",
              returnType    = SuggestionBuilder.Any,
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Conversion(
              externalId = None,
              module     = "Unnamed.Test",
              arguments = Seq(
                Suggestion
                  .Argument("opt", "Unnamed.Test.MyMaybe", false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              selfType      = "Unnamed.Test.MyMaybe",
              documentation = Some(" My conversion method")
            ),
            Vector()
          )
        )
      )
    }

    "build function simple" in {

      val code =
        """main =
          |    foo a = a + 1
          |    foo 42
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion
                      .Argument("a", SuggestionBuilder.Any, false, false, None)
                  ),
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(2, 10)
                  ),
                  None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build function with complex body" in {

      val code =
        """main =
          |    foo a =
          |        b = a + 1
          |        b
          |    foo 42
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion
                      .Argument("a", SuggestionBuilder.Any, false, false, None)
                  ),
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(4, 10)
                  ),
                  documentation = None
                ),
                Vector(
                  Tree.Node(
                    Suggestion.Local(
                      externalId = None,
                      module     = "Unnamed.Test",
                      name       = "b",
                      returnType = SuggestionBuilder.Any,
                      scope = Suggestion.Scope(
                        Suggestion.Position(1, 11),
                        Suggestion.Position(3, 9)
                      ),
                      documentation = None
                    ),
                    Vector()
                  )
                )
              )
            )
          )
        )
      )
    }

    "build function with associated type signature" in {

      val code =
        """main =
          |    foo : Number -> Number
          |    foo a = a + 1
          |    foo 42
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion.Argument("a", "Number", false, false, None)
                  ),
                  returnType = "Number",
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(3, 10)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build function with resolved type signature" in {

      val code =
        """type A
          |
          |main =
          |    foo : A -> A
          |    foo a = a + 1
          |    foo 42
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              params        = Seq(),
              returnType    = "Unnamed.Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion
                      .Argument(
                        "a",
                        "Unnamed.Test.A",
                        false,
                        false,
                        None,
                        Some(Seq("Unnamed.Test.A"))
                      )
                  ),
                  returnType = "Unnamed.Test.A",
                  scope = Suggestion.Scope(
                    Suggestion.Position(2, 6),
                    Suggestion.Position(5, 10)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build function with documentation" in {

      val code =
        """main =
          |    ## Foo documentation.
          |    foo a = a + 1
          |    foo 42
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion
                      .Argument(
                        "a",
                        SuggestionBuilder.Any,
                        false,
                        false,
                        None
                      )
                  ),
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(3, 10)
                  ),
                  documentation = Some(" Foo documentation.")
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local simple" in {

      val code =
        """main =
          |    foo = 42
          |    foo
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(2, 7)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local with complex body" in {

      val code =
        """main =
          |    foo =
          |        b = 42
          |        b
          |    foo
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(4, 7)
                  ),
                  documentation = None
                ),
                Vector(
                  Tree.Node(
                    Suggestion.Local(
                      externalId = None,
                      module     = "Unnamed.Test",
                      name       = "b",
                      returnType = SuggestionBuilder.Any,
                      scope = Suggestion.Scope(
                        Suggestion.Position(1, 9),
                        Suggestion.Position(3, 9)
                      ),
                      documentation = None
                    ),
                    Vector()
                  )
                )
              )
            )
          )
        )
      )
    }

    "build local with associated type signature" in {

      val code =
        """main =
          |    foo : Number
          |    foo = 42
          |    foo
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  returnType = "Number",
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(3, 7)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local with resolved type signature" in {

      val code =
        """type A
          |
          |main =
          |    foo : A
          |    foo = A
          |    foo
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              params        = Seq(),
              returnType    = "Unnamed.Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  returnType = "Unnamed.Test.A",
                  scope = Suggestion.Scope(
                    Suggestion.Position(2, 6),
                    Suggestion.Position(5, 7)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local with documentation" in {

      val code =
        """main =
          |    ## This is foo.
          |    foo = 42
          |    foo
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(3, 7)
                  ),
                  documentation = Some(" This is foo.")
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build type simple" in {

      val code =
        """type MyType
          |    MkMyType a b""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyType",
              params        = Seq(),
              returnType    = "Unnamed.Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MkMyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with documentation" in {

      val code =
        """## Module doc
          |
          |## My sweet type
          |type Mtp
          |    ## My sweet type
          |    MyType a b""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Mtp",
              params        = Seq(),
              returnType    = "Unnamed.Test.Mtp",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = Some(" My sweet type")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.Mtp",
              documentation = Some(" My sweet type"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.Mtp", false, false, None)
              ),
              selfType      = "Unnamed.Test.Mtp",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.Mtp", false, false, None)
              ),
              selfType      = "Unnamed.Test.Mtp",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with two constructors" in {

      val code =
        """type Maybe
          |    Nothing
          |    Just a""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Maybe",
              params        = Seq(),
              returnType    = "Unnamed.Test.Maybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.Maybe", false, false, None)
              ),
              selfType      = "Unnamed.Test.Maybe",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with documentation and two constructors" in {

      val code =
        """## Module doc
          |
          |## When in doubt
          |type Maybe
          |    ## Nothing here
          |    Nothing
          |    ## Something there
          |    Just a""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Maybe",
              params        = Seq(),
              returnType    = "Unnamed.Test.Maybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = Some(" When in doubt")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Maybe",
              documentation = Some(" Nothing here"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.Maybe",
              documentation = Some(" Something there"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.Maybe", false, false, None)
              ),
              selfType      = "Unnamed.Test.Maybe",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with methods, type signatures and docs" in {
      val code =
        """type List
          |    ## And more
          |    Cons
          |    ## End
          |    Nil
          |
          |    ## a method
          |    empty : List
          |    empty = Nil
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "List",
              params        = Seq(),
              returnType    = "Unnamed.Test.List",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Cons",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.List",
              documentation = Some(" And more"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nil",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.List",
              documentation = Some(" End"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "empty",
              arguments     = Seq(),
              selfType      = "Unnamed.Test.List",
              returnType    = "Unnamed.Test.List",
              isStatic      = true,
              documentation = Some(" a method"),
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with methods, without type signatures" in {
      val code =
        """type Maybe
          |    Nothing
          |    Just a
          |
          |    map self f = case self of
          |        Just a  -> Just (f a)
          |        Nothing -> Nothing""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Maybe",
              params        = Seq(),
              returnType    = "Unnamed.Test.Maybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.Maybe", false, false, None)
              ),
              selfType      = "Unnamed.Test.Maybe",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "map",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.Maybe", false, false, None),
                Suggestion
                  .Argument("f", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test.Maybe",
              returnType    = SuggestionBuilder.Any,
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with ascribed constructor" in {

      val code =
        """type S
          |  X
          |  Y
          |
          |type T
          |    A (x : S)
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "S",
              params        = Seq(),
              returnType    = "Unnamed.Test.S",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "X",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Y",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Unnamed.Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "A",
              arguments = Seq(
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.S",
                  false,
                  false,
                  None,
                  Some(Seq("Unnamed.Test.S.X", "Unnamed.Test.S.Y"))
                )
              ),
              returnType    = "Unnamed.Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.T", false, false, None)
              ),
              selfType      = "Unnamed.Test.T",
              returnType    = "Unnamed.Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with constructors with annotations" in {

      val code =
        """type S
          |  @a foo
          |  @b bar
          |  X a b
          |  Y c
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "S",
              params        = Seq(),
              returnType    = "Unnamed.Test.S",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "X",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.S",
              documentation = None,
              annotations   = Seq("a", "b")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Y",
              arguments = Seq(
                Suggestion
                  .Argument("c", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.S", false, false, None)
              ),
              selfType      = "Unnamed.Test.S",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.S", false, false, None)
              ),
              selfType      = "Unnamed.Test.S",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "c",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.S", false, false, None)
              ),
              selfType      = "Unnamed.Test.S",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with qualified ascribed constructor" in {

      val code =
        """import Standard.Base.Data.Numbers
          |
          |type T
          |    A (x : Numbers.Number)
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Unnamed.Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "A",
              arguments = Seq(
                Suggestion
                  .Argument(
                    "x",
                    "Standard.Base.Data.Numbers.Number",
                    false,
                    false,
                    None
                  )
              ),
              returnType    = "Unnamed.Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.T", false, false, None)
              ),
              selfType      = "Unnamed.Test.T",
              returnType    = "Standard.Base.Data.Numbers.Number",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with ascribed type parameter in constructor" in {

      val code =
        """
          |type E a b
          |    L (x : a)
          |    R (y : b)
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "E",
              params = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.E",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "L",
              arguments = Seq(
                Suggestion.Argument("x", "a", false, false, None)
              ),
              returnType    = "Unnamed.Test.E",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "R",
              arguments = Seq(
                Suggestion.Argument("y", "b", false, false, None)
              ),
              returnType    = "Unnamed.Test.E",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.E", false, false, None)
              ),
              selfType      = "Unnamed.Test.E",
              returnType    = "a",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "y",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.E", false, false, None)
              ),
              selfType      = "Unnamed.Test.E",
              returnType    = "b",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with constructor with internal field" in {

      val code =
        """import Standard.Base.Data.Numbers
          |
          |type T
          |    A a_internal internal_a x
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Unnamed.Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "A",
              arguments = Seq(
                Suggestion.Argument(
                  "a_internal",
                  SuggestionBuilder.Any,
                  false,
                  false,
                  None
                ),
                Suggestion.Argument(
                  "internal_a",
                  SuggestionBuilder.Any,
                  false,
                  false,
                  None
                ),
                Suggestion.Argument(
                  "x",
                  SuggestionBuilder.Any,
                  false,
                  false,
                  None
                )
              ),
              returnType    = "Unnamed.Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Unnamed.Test.T", false, false, None)
              ),
              selfType      = "Unnamed.Test.T",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build Integer type" in {

      val code = "type Integer"
      val moduleName =
        QualifiedName.fromString("Standard.Base.Data.Numbers")
      val module = code.preprocessModule(moduleName)

      build(code, module, moduleName) shouldEqual Tree.Root(
        Vector(
          Tree.Node(
            Suggestion.Module(
              module        = moduleName.toString,
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = moduleName.toString,
              name          = "Integer",
              params        = Seq(),
              returnType    = moduleName.createChild("Integer").toString,
              parentType    = Some(moduleName.createChild("Number").toString),
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build Number type" in {

      val code = "type Number"
      val moduleName =
        QualifiedName.fromString("Standard.Base.Data.Numbers")
      val module = code.preprocessModule(moduleName)

      build(code, module, moduleName) shouldEqual Tree.Root(
        Vector(
          Tree.Node(
            Suggestion.Module(
              module        = moduleName.toString,
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = moduleName.toString,
              name          = "Number",
              params        = Seq(),
              returnType    = moduleName.createChild("Number").toString,
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with simple type" in {
      val code =
        """type MyType
          |    MkMyType a b
          |
          |main = IO.println "Hello!"""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyType",
              params        = Seq(),
              returnType    = "Unnamed.Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MkMyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build module with a type named as module" in {
      val code =
        """type Test
          |    Mk_Test a
          |
          |main = IO.println "Hello!"""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Test",
              params        = Seq(),
              returnType    = "Unnamed.Test.Test",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Mk_Test",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.Test",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Unnamed.Test.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build module with overloaded functions and two constructors" in {
      val code =
        """type A
          |    Mk_A
          |    Mk_A_Plus a
          |
          |    quux : A -> A
          |    quux self x = x
          |
          |quux : A -> A
          |quux x = x
          |
          |main =
          |    quux A
          |    A.quux A""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              params        = List(),
              returnType    = "Unnamed.Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Mk_A",
              arguments     = List(),
              returnType    = "Unnamed.Test.A",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Mk_A_Plus",
              arguments = List(
                Suggestion.Argument(
                  "a",
                  "Standard.Base.Any.Any",
                  false,
                  false,
                  None,
                  None
                )
              ),
              returnType    = "Unnamed.Test.A",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = Vector(
                Suggestion
                  .Argument("self", "Unnamed.Test.A", false, false, None, None)
              ),
              selfType      = "Unnamed.Test.A",
              returnType    = "Standard.Base.Any.Any",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              None,
              "Unnamed.Test",
              "quux",
              Vector(
                Suggestion
                  .Argument("self", "Unnamed.Test.A", false, false, None),
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  Some(List("Unnamed.Test.A.Mk_A", "Unnamed.Test.A.Mk_A_Plus"))
                )
              ),
              selfType      = "Unnamed.Test.A",
              returnType    = "Unnamed.Test.A",
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "quux",
              arguments = Vector(
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  Some(List("Unnamed.Test.A.Mk_A", "Unnamed.Test.A.Mk_A_Plus"))
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Unnamed.Test.A",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = List(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "single constructor isn't suggested" in {
      val code =
        """type A
          |    Mk_A
          |
          |    quux : A -> A
          |    quux self x = x
          |
          |quux : A -> A
          |quux x = x
          |
          |main =
          |    quux A
          |    A.quux A""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              params        = List(),
              returnType    = "Unnamed.Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Mk_A",
              arguments     = List(),
              returnType    = "Unnamed.Test.A",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "quux",
              arguments = Vector(
                Suggestion
                  .Argument("self", "Unnamed.Test.A", false, false, None),
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  None
                )
              ),
              selfType      = "Unnamed.Test.A",
              returnType    = "Unnamed.Test.A",
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "quux",
              arguments = Vector(
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  None
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Unnamed.Test.A",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = List(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build method with external id" in {
      val code =
        """main = IO.println "Hello!"
          |
          |
          |#### METADATA ####
          |[[{"index": {"value": 7}, "size": {"value": 19}}, "4083ce56-a5e5-4ecd-bf45-37ddf0b58456"]]
          |[]
          |""".stripMargin.linesIterator.mkString("\n")
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId =
                Some(UUID.fromString("4083ce56-a5e5-4ecd-bf45-37ddf0b58456")),
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build function with external id" in {
      val code =
        """main =
          |    id x = x
          |    IO.println (id "Hello!")
          |
          |
          |#### METADATA ####
          |[[{"index": {"value": 18}, "size": {"value": 1}}, "f533d910-63f8-44cd-9204-a1e2d46bb7c3"]]
          |[]
          |""".stripMargin.linesIterator.mkString("\n")
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = Some(
                    UUID.fromString("f533d910-63f8-44cd-9204-a1e2d46bb7c3")
                  ),
                  module = "Unnamed.Test",
                  name   = "id",
                  arguments = Seq(
                    Suggestion
                      .Argument("x", SuggestionBuilder.Any, false, false, None)
                  ),
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(2, 28)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local with external id" in {
      val code =
        """main =
          |    foo = 42
          |    IO.println foo
          |
          |
          |#### METADATA ####
          |[[{"index": {"value": 17}, "size": {"value": 2}}, "0270bcdf-26b8-4b99-8745-85b3600c7359"]]
          |[]
          |""".stripMargin.linesIterator.mkString("\n")
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = Some(
                    UUID.fromString("0270bcdf-26b8-4b99-8745-85b3600c7359")
                  ),
                  module     = "Unnamed.Test",
                  name       = "foo",
                  returnType = SuggestionBuilder.Any,
                  scope = Suggestion.Scope(
                    Suggestion.Position(0, 6),
                    Suggestion.Position(2, 18)
                  ),
                  documentation = None
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build module with documentation" in {

      val code =
        """## Module doc
          |
          |## The foo
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          Tree.Node(
            Suggestion.Module(
              module        = "Unnamed.Test",
              documentation = Some(" Module doc")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = Some(" The foo"),
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "provide type variants when applicable" in {
      val code =
        """type My_Tp
          |    Variant_A
          |    Variant_B
          |
          |foo : My_Tp -> My_Tp
          |foo arg = arg.do_sth""".stripMargin
      val module      = code.preprocessModule
      val suggestions = build(code, module)
      val fooSuggestion = suggestions.collectFirst {
        case s: Suggestion.DefinedMethod if s.name == "foo" => s
      }
      val fooArg = fooSuggestion.get.arguments(0)
      fooArg.reprType shouldEqual "Unnamed.Test.My_Tp"
      fooArg.tagValues shouldEqual Some(
        List("Unnamed.Test.My_Tp.Variant_A", "Unnamed.Test.My_Tp.Variant_B")
      )
    }

    "parse Text.trim properly" in {
      val code =
        """|import Standard.Base.Data.Text.Text
           |
           |Text.trim : (Location.Start | Location.End | Location.Both) -> (Text | (Text -> Boolean)) -> Text
           |Text.trim self where=Location.Both what=_.is_whitespace = self
           |""".stripMargin
      val module      = code.preprocessModule
      val suggestions = build(code, module)
      val method = suggestions.collectFirst {
        case s: Suggestion.DefinedMethod if s.name == "trim" => s
      }
      method.get.arguments.size shouldEqual 3
      val arg1 = method.get.arguments(1)
      arg1.reprType shouldEqual "Location.Start | Location.End | Location.Both"
      arg1.tagValues shouldEqual Some(
        List("Location.Start", "Location.End", "Location.Both")
      )
      val arg2 = method.get.arguments(2)
      arg2.reprType shouldEqual "Standard.Base.Data.Text.Text | (Standard.Base.Data.Text.Text -> Boolean)"
      arg2.tagValues shouldEqual None
    }
  }

  private def build(
    source: String,
    ir: IR.Module,
    module: QualifiedName = Module
  ): Tree.Root[Suggestion] =
    SuggestionBuilder(source, langCtx.getCompiler).build(module, ir)
}
