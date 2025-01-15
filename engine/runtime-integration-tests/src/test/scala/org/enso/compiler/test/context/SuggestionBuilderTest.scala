package org.enso.compiler.test.context

import com.oracle.truffle.api.TruffleFile
import org.enso.compiler.suggestions.SuggestionBuilder
import org.enso.compiler.core.ir.Module
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.common.{LanguageInfo, MethodNames}
import org.enso.editions.LibraryName
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.Tree
import org.enso.pkg.Package
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class SuggestionBuilderTest extends AnyWordSpecLike with Matchers {
  private val ctx = new InterpreterContext()
  private val langCtx = ctx
    .ctx()
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  implicit private class PreprocessModule(code: String) {

    def preprocessModule(moduleName: QualifiedName = Module): Module = {
      var pkg: Package[TruffleFile] = null
      if (!moduleName.isSimple()) {
        val libName = LibraryName.fromModuleName(moduleName.toString)
        libName shouldBe defined
        val pkgOpt =
          langCtx.getPackageRepository.getPackageForLibrary(libName.get)
        pkgOpt shouldBe defined
        pkg = pkgOpt.get
      }
      val module = new runtime.Module(
        moduleName,
        pkg,
        code.stripMargin.linesIterator.mkString("\n")
      )
      langCtx.getCompiler.run(module.asCompilerModule())
      module.getIr
    }

  }

  private val Module = QualifiedName.simpleName("Test")
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

  "SuggestionBuilder" should {

    "build method without explicit arguments" in {

      val code   = """foo = 42"""
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
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
              selfType      = "Test",
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

    "build method with an ascribed argument" in {

      val code =
        """import Standard.Base.Data.Text.Text
          |
          |foo (a : Text) = 42""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
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
              selfType      = "Test",
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

    "build method with an ascribed and default argument" in {

      val code =
        """import Standard.Base.Data.Text.Text
          |
          |foo (a : Text = "42") = a""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Standard.Base.Data.Text.Text",
                  false,
                  true,
                  Some("\"42\"")
                )
              ),
              selfType      = "Test",
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

    "build method with a type containing higher kinds" in {

      val code =
        """
          |foo : Either (Vector Number) Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
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
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  "Test",
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
                  "Test",
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

      val code   = """foo (a = 0) (b = "bar") (c = x.y) = a + b + c"""
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, true, Some("0")),
                Suggestion
                  .Argument(
                    "b",
                    SuggestionBuilder.Any,
                    false,
                    true,
                    Some("\"bar\"")
                  ),
                Suggestion
                  .Argument(
                    "c",
                    SuggestionBuilder.Any,
                    false,
                    true,
                    Some("x.y")
                  )
              ),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "MyType",
              params        = Seq(),
              returnType    = "Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "bar",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.MyType", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Test.MyType",
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
      val module = code.preprocessModule()

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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId = None,
              module     = "Test",
              name       = "MyAtom",
              params = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.MyAtom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "bar",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.MyAtom", false, false, None),
                Suggestion.Argument("a", "Number", false, false, None),
                Suggestion.Argument("b", "Number", false, false, None)
              ),
              selfType      = "Test.MyAtom",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "MyAtom",
              params        = Seq(),
              returnType    = "Test.MyAtom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "apply",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.MyAtom", false, false, None),
                Suggestion.Argument("f", "Number -> Number", false, false, None)
              ),
              selfType      = "Test.MyAtom",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "My_Atom",
              params        = Seq(),
              returnType    = "Test.My_Atom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Variant_1",
              arguments     = Seq(),
              returnType    = "Test.My_Atom",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Variant_2",
              arguments     = Seq(),
              returnType    = "Test.My_Atom",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Other_Atom",
              params        = Seq(),
              returnType    = "Test.Other_Atom",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "apply",
              arguments = Seq(
                Suggestion
                  .Argument(
                    "self",
                    "Test.Other_Atom",
                    false,
                    false,
                    None
                  ),
                Suggestion.Argument(
                  "f",
                  "Number | Test.Other_Atom | Test.My_Atom",
                  false,
                  false,
                  None,
                  Some(
                    Seq(
                      "Number",
                      "Test.Other_Atom",
                      "Test.My_Atom.Variant_1",
                      "Test.My_Atom.Variant_2"
                    )
                  )
                )
              ),
              selfType      = "Test.Other_Atom",
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
          |foo : Text | Boolean | Value | Auto -> Value
          |foo a = a
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Value",
              params        = Seq(),
              returnType    = "Test.Value",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Auto",
              params        = Seq(),
              returnType    = "Test.Auto",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Standard.Base.Data.Text.Text | Standard.Base.Data.Boolean.Boolean | Test.Value | Test.Auto",
                  false,
                  false,
                  None,
                  Some(
                    Seq(
                      "Standard.Base.Data.Boolean.Boolean.True",
                      "Standard.Base.Data.Boolean.Boolean.False",
                      "Test.Value.A",
                      "Test.Value.B",
                      "Test.Auto"
                    )
                  )
                )
              ),
              selfType      = "Test",
              returnType    = "Test.Value",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build argument tag values from ascribed type" in {

      val code =
        """type Value
          |    A
          |    B
          |
          |type Auto
          |
          |foo (a : Value | Auto) = a
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Value",
              params        = Seq(),
              returnType    = "Test.Value",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Auto",
              params        = Seq(),
              returnType    = "Test.Auto",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Test.Value | Test.Auto",
                  false,
                  false,
                  None,
                  Some(Seq("..A", "..B", "Test.Auto"))
                )
              ),
              selfType      = "Test",
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

    "build autoscope tagValues for Boolean" in {

      val code =
        """from Standard.Base import all
          |
          |type Value
          |    A
          |    B
          |
          |foo (a : Value | Boolean) = a
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Value",
              params        = Seq(),
              returnType    = "Test.Value",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Test.Value | Standard.Base.Data.Boolean.Boolean",
                  false,
                  false,
                  None,
                  Some(Seq("..A", "..B", "True", "False"))
                )
              ),
              selfType      = "Test",
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

    "build argument tag values from ascribed type and type signature" in {

      val code =
        """type Value
          |    A
          |    B
          |
          |type Auto
          |
          |foo : Value | Auto -> Value | Auto
          |foo (a : Value | Auto) = a
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Value",
              params        = Seq(),
              returnType    = "Test.Value",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Test.Value",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Auto",
              params        = Seq(),
              returnType    = "Test.Auto",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Test.Value | Test.Auto",
                  false,
                  false,
                  None,
                  Some(Seq("..A", "..B", "Test.Auto"))
                )
              ),
              selfType      = "Test",
              returnType    = "Test.Value | Test.Auto",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build argument tag values checking if autoscoped constructors are distinct" in {

      val code =
        """type T
          |    A
          |    B
          |
          |type K
          |    B
          |    C
          |
          |foo (a : T | K) = a
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "K",
              params        = Seq(),
              returnType    = "Test.K",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "B",
              arguments     = Seq(),
              returnType    = "Test.K",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "C",
              arguments     = Seq(),
              returnType    = "Test.K",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Test.T | Test.K",
                  false,
                  false,
                  None,
                  Some(
                    Seq(
                      "Test.T.A",
                      "Test.T.B",
                      "Test.K.B",
                      "Test.K.C"
                    )
                  )
                )
              ),
              selfType      = "Test",
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

    "build method with lazy arguments" in {

      val code =
        """foo ~a = a + 1"""
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, true, false, None)
              ),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "A",
              params        = Seq(),
              returnType    = "Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument(
                  "a",
                  "Test.A",
                  false,
                  false,
                  None,
                  Some(Seq("Test.A"))
                )
              ),
              selfType      = "Test",
              returnType    = "Test.A",
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
      val code =
        """import Standard.Base.Data.Numbers
          |
          |type Foo
          |    Value foo
          |
          |## My conversion
          |Foo.from (that:Numbers.Number) = Foo.Value a
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Foo",
              params        = Seq(),
              returnType    = "Test.Foo",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Value",
              arguments = Seq(
                Suggestion
                  .Argument("foo", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Foo",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Foo", false, false, None)
              ),
              selfType      = "Test.Foo",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Conversion(
              externalId = None,
              module     = "Test",
              arguments = Seq(
                Suggestion.Argument(
                  "that",
                  "Standard.Base.Data.Numbers.Number",
                  false,
                  false,
                  None
                )
              ),
              selfType      = "Standard.Base.Data.Numbers.Number",
              returnType    = "Test.Foo",
              documentation = Some(" My conversion")
            ),
            Vector()
          )
        )
      )
    }

    "build conversion method with extra arguments" in {
      val code =
        """import Standard.Base.Data.Numbers
          |
          |type Foo
          |    Value foo bar
          |
          |Foo.from (that:Numbers.Number) other=1 = Foo.Value that other
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Foo",
              params        = Seq(),
              returnType    = "Test.Foo",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Value",
              arguments = Seq(
                Suggestion
                  .Argument("foo", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("bar", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Foo",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Foo", false, false, None)
              ),
              selfType      = "Test.Foo",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "bar",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Foo", false, false, None)
              ),
              selfType      = "Test.Foo",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Conversion(
              externalId = None,
              module     = "Test",
              arguments = Seq(
                Suggestion.Argument(
                  "that",
                  "Standard.Base.Data.Numbers.Number",
                  false,
                  false,
                  None
                ),
                Suggestion
                  .Argument(
                    "other",
                    SuggestionBuilder.Any,
                    false,
                    true,
                    Some("1")
                  )
              ),
              selfType      = "Standard.Base.Data.Numbers.Number",
              returnType    = "Test.Foo",
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build conversion method with extra typed arguments" in {
      val code =
        """import Standard.Base.Data.Numbers
          |from Standard.Base.Data.Boolean import Boolean
          |
          |type Foo
          |    Value foo bar
          |
          |Foo.from (that:Numbers.Number) (other:Boolean=Boolean.True) = Foo.Value that other
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Foo",
              params        = Seq(),
              returnType    = "Test.Foo",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Value",
              arguments = Seq(
                Suggestion
                  .Argument("foo", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("bar", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Foo",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Foo", false, false, None)
              ),
              selfType      = "Test.Foo",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "bar",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Foo", false, false, None)
              ),
              selfType      = "Test.Foo",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Conversion(
              externalId = None,
              module     = "Test",
              arguments = Seq(
                Suggestion.Argument(
                  "that",
                  "Standard.Base.Data.Numbers.Number",
                  false,
                  false,
                  None
                ),
                Suggestion
                  .Argument(
                    "other",
                    "Standard.Base.Data.Boolean.Boolean",
                    false,
                    true,
                    Some("Boolean.True"),
                    Some(
                      List(
                        "True",
                        "False"
                      )
                    )
                  )
              ),
              selfType      = "Standard.Base.Data.Numbers.Number",
              returnType    = "Test.Foo",
              documentation = None
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Test",
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
                      module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "A",
              params        = Seq(),
              returnType    = "Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion
                      .Argument(
                        "a",
                        "Test.A",
                        false,
                        false,
                        None,
                        Some(Seq("Test.A"))
                      )
                  ),
                  returnType = "Test.A",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Test",
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
                      module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "A",
              params        = Seq(),
              returnType    = "Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Test",
                  name       = "foo",
                  returnType = "Test.A",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
              returnType    = SuggestionBuilder.Any,
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "MyType",
              params        = Seq(),
              returnType    = "Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "MkMyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.MyType",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.MyType", false, false, None)
              ),
              selfType      = "Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.MyType", false, false, None)
              ),
              selfType      = "Test.MyType",
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
          |    ## My sweet value
          |    MyType a b""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Mtp",
              params        = Seq(),
              returnType    = "Test.Mtp",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = Some(" My sweet type")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Mtp",
              documentation = Some(" My sweet value"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Mtp", false, false, None)
              ),
              selfType      = "Test.Mtp",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Mtp", false, false, None)
              ),
              selfType      = "Test.Mtp",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Maybe",
              params        = Seq(),
              returnType    = "Test.Maybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Maybe", false, false, None)
              ),
              selfType      = "Test.Maybe",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Maybe",
              params        = Seq(),
              returnType    = "Test.Maybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = Some(" When in doubt")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Test.Maybe",
              documentation = Some(" Nothing here"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Maybe",
              documentation = Some(" Something there"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Maybe", false, false, None)
              ),
              selfType      = "Test.Maybe",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "List",
              params        = Seq(),
              returnType    = "Test.List",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Cons",
              arguments     = Seq(),
              returnType    = "Test.List",
              documentation = Some(" And more"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Nil",
              arguments     = Seq(),
              returnType    = "Test.List",
              documentation = Some(" End"),
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "empty",
              arguments     = Seq(),
              selfType      = "Test.List",
              returnType    = "Test.List",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Maybe",
              params        = Seq(),
              returnType    = "Test.Maybe",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Maybe",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Maybe", false, false, None)
              ),
              selfType      = "Test.Maybe",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "map",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.Maybe", false, false, None),
                Suggestion
                  .Argument("f", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Test.Maybe",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "S",
              params        = Seq(),
              returnType    = "Test.S",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "X",
              arguments     = Seq(),
              returnType    = "Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Y",
              arguments     = Seq(),
              returnType    = "Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "A",
              arguments = Seq(
                Suggestion.Argument(
                  "x",
                  "Test.S",
                  false,
                  false,
                  None,
                  Some(Seq("..X", "..Y"))
                )
              ),
              returnType    = "Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.T", false, false, None)
              ),
              selfType      = "Test.T",
              returnType    = "Test.S",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "S",
              params        = Seq(),
              returnType    = "Test.S",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "X",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.S",
              documentation = None,
              annotations   = Seq("a", "b")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Y",
              arguments = Seq(
                Suggestion
                  .Argument("c", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.S",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.S", false, false, None)
              ),
              selfType      = "Test.S",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "b",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.S", false, false, None)
              ),
              selfType      = "Test.S",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "c",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.S", false, false, None)
              ),
              selfType      = "Test.S",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
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
              returnType    = "Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.T", false, false, None)
              ),
              selfType      = "Test.T",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId = None,
              module     = "Test",
              name       = "E",
              params = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.E",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "L",
              arguments = Seq(
                Suggestion.Argument("x", "a", false, false, None)
              ),
              returnType    = "Test.E",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "R",
              arguments = Seq(
                Suggestion.Argument("y", "b", false, false, None)
              ),
              returnType    = "Test.E",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.E", false, false, None)
              ),
              selfType      = "Test.E",
              returnType    = "a",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "y",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.E", false, false, None)
              ),
              selfType      = "Test.E",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
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
              returnType    = "Test.T",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "x",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.T", false, false, None)
              ),
              selfType      = "Test.T",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          )
        )
      )
    }

    "build type with private constructor" in {
      val code =
        """type T
          |    private A x y
          |
          |    foo self = x + y
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "foo",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.T", false, false, None)
              ),
              selfType      = "Test.T",
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

    "build type with private methods" in {
      val code =
        """type T
          |    private priv_meth self = 42
          |    pub_meth self = 42
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "T",
              params        = Seq(),
              returnType    = "Test.T",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "pub_meth",
              arguments = Seq(
                Suggestion
                  .Argument("self", "Test.T", false, false, None)
              ),
              selfType      = "Test.T",
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

    "build module with private methods" in {
      val code =
        """
          |private priv_stat_method x = 42
          |
          |pub_stat_method x = 42
          |""".stripMargin

      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "pub_stat_method",
              arguments = Seq(
                Suggestion
                  .Argument("x", "Standard.Base.Any.Any", false, false, None)
              ),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "MyType",
              params        = Seq(),
              returnType    = "Test.MyType",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "MkMyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.MyType",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.MyType", false, false, None)
              ),
              selfType      = "Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.MyType", false, false, None)
              ),
              selfType      = "Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
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

    "build private module" in {
      val code =
        """private
          |
          |type T
          |
          |main = "Hello World!"
          |""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(Vector())
    }

    "build module with a type named as module" in {
      val code =
        """type Test
          |    Mk_Test a
          |
          |main = IO.println "Hello!"""".stripMargin
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "Test",
              params        = Seq(),
              returnType    = "Test.Test",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
              name       = "Mk_Test",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Test.Test",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("self", "Test.Test", false, false, None)
              ),
              selfType      = "Test.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "A",
              params        = List(),
              returnType    = "Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Mk_A",
              arguments     = List(),
              returnType    = "Test.A",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId = None,
              module     = "Test",
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
              returnType    = "Test.A",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Getter(
              externalId = None,
              module     = "Test",
              name       = "a",
              arguments = Vector(
                Suggestion
                  .Argument("self", "Test.A", false, false, None, None)
              ),
              selfType      = "Test.A",
              returnType    = "Standard.Base.Any.Any",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              None,
              "Test",
              "quux",
              Vector(
                Suggestion
                  .Argument("self", "Test.A", false, false, None),
                Suggestion.Argument(
                  "x",
                  "Test.A",
                  false,
                  false,
                  None,
                  Some(List("Test.A.Mk_A", "Test.A.Mk_A_Plus"))
                )
              ),
              selfType      = "Test.A",
              returnType    = "Test.A",
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "quux",
              arguments = Vector(
                Suggestion.Argument(
                  "x",
                  "Test.A",
                  false,
                  false,
                  None,
                  Some(List("Test.A.Mk_A", "Test.A.Mk_A_Plus"))
                )
              ),
              selfType      = "Test",
              returnType    = "Test.A",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = List(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Type(
              externalId    = None,
              module        = "Test",
              name          = "A",
              params        = List(),
              returnType    = "Test.A",
              parentType    = Some(SuggestionBuilder.Any),
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Constructor(
              externalId    = None,
              module        = "Test",
              name          = "Mk_A",
              arguments     = List(),
              returnType    = "Test.A",
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "quux",
              arguments = Vector(
                Suggestion
                  .Argument("self", "Test.A", false, false, None),
                Suggestion.Argument(
                  "x",
                  "Test.A",
                  false,
                  false,
                  None,
                  None
                )
              ),
              selfType      = "Test.A",
              returnType    = "Test.A",
              isStatic      = false,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId = None,
              module     = "Test",
              name       = "quux",
              arguments = Vector(
                Suggestion.Argument(
                  "x",
                  "Test.A",
                  false,
                  false,
                  None,
                  None
                )
              ),
              selfType      = "Test",
              returnType    = "Test.A",
              isStatic      = true,
              documentation = None,
              annotations   = Seq()
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = List(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId =
                Some(UUID.fromString("4083ce56-a5e5-4ecd-bf45-37ddf0b58456")),
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
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
                  module = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "main",
              arguments     = Seq(),
              selfType      = "Test",
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
                  module     = "Test",
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
      val module = code.preprocessModule()

      build(code, module) shouldEqual Tree.Root(
        Vector(
          Tree.Node(
            Suggestion.Module(
              module        = "Test",
              documentation = Some(" Module doc")
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.DefinedMethod(
              externalId    = None,
              module        = "Test",
              name          = "foo",
              arguments     = Seq(),
              selfType      = "Test",
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
      val module      = code.preprocessModule()
      val suggestions = build(code, module)
      val fooSuggestion = suggestions.collectFirst {
        case s: Suggestion.DefinedMethod if s.name == "foo" => s
      }
      val fooArg = fooSuggestion.get.arguments(0)
      fooArg.reprType shouldEqual "Test.My_Tp"
      fooArg.tagValues shouldEqual Some(
        List("Test.My_Tp.Variant_A", "Test.My_Tp.Variant_B")
      )
    }

    "parse Text.trim properly" in {
      val code =
        """|import Standard.Base.Data.Text.Text
           |
           |Text.trim : (Location.Start | Location.End | Location.Both) -> (Text | (Text -> Boolean)) -> Text
           |Text.trim self where=Location.Both what=_.is_whitespace = self
           |""".stripMargin
      val module      = code.preprocessModule()
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
    ir: Module,
    module: QualifiedName = Module
  ): Tree.Root[Suggestion] = {
    val compiler = langCtx.getCompiler
    val types =
      org.enso.interpreter.runtime.Module.findTypeHierarchy(compiler.context)
    SuggestionBuilder(source, types, compiler).build(module, ir)
  }
}
