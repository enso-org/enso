package org.enso.compiler.test.context

import java.util.UUID

import org.enso.compiler.Passes
import org.enso.compiler.context.{
  FreshNameSupply,
  ModuleContext,
  SuggestionBuilder
}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassManager
import org.enso.compiler.test.CompilerTest
import org.enso.pkg.QualifiedName
import org.enso.polyglot.Suggestion

class SuggestionBuilderTest extends CompilerTest {

  implicit val passManager: PassManager = new Passes().passManager

  "SuggestionBuilder" should {

    "build method without explicit arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """foo = 42"""
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build method with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """## The foo
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = Some(" The foo")
        )
      )
    }

    "build method with type and documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """## The foo
          |foo : Number
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Test", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Number",
          documentation = Some(" The foo")
        )
      )
    }

    "build method with arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """foo a b =
          |    x : Number
          |    x = a + 1
          |    y = b - 2
          |    x * y""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Local(
          externalId = None,
          "Unnamed.Test",
          "x",
          "Number",
          Suggestion.Scope(Suggestion.Position(0, 9), Suggestion.Position(4, 9))
        ),
        Suggestion.Local(
          externalId = None,
          "Unnamed.Test",
          "y",
          "Any",
          Suggestion.Scope(Suggestion.Position(0, 9), Suggestion.Position(4, 9))
        )
      )
    }

    "build method with default arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """foo (a = 0) = a + 1"""
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", false, true, Some("0"))
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build method with explicit self type" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type MyType
          |
          |MyType.bar a b = a + b
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "MyType",
          arguments     = Seq(),
          returnType    = "MyType",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "bar",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          selfType      = "MyType",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "not build method with undefined self type" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """MyAtom.bar a b = a + b"""
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq()
    }

    "build method with associated type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type MyAtom
          |
          |## My bar
          |MyAtom.bar : Number -> Number -> Number
          |MyAtom.bar a b = a + b
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "MyAtom",
          arguments     = Seq(),
          returnType    = "MyAtom",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "bar",
          arguments = Seq(
            Suggestion.Argument("this", "MyAtom", false, false, None),
            Suggestion.Argument("a", "Number", false, false, None),
            Suggestion.Argument("b", "Number", false, false, None)
          ),
          selfType      = "MyAtom",
          returnType    = "Number",
          documentation = Some(" My bar")
        )
      )
    }

    "build method with function type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type MyAtom
          |
          |MyAtom.apply : (Number -> Number) -> Number
          |MyAtom.apply f = f this
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "MyAtom",
          arguments     = Seq(),
          returnType    = "MyAtom",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "apply",
          arguments = Seq(
            Suggestion.Argument("this", "MyAtom", false, false, None),
            Suggestion.Argument("f", "Number -> Number", false, false, None)
          ),
          selfType      = "MyAtom",
          returnType    = "Number",
          documentation = None
        )
      )
    }

    "build method with lazy arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """foo ~a = a + 1"""
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", true, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build function" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo a = a + 1
          |    foo 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Function(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "foo",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType = "Any",
          scope = Suggestion.Scope(
            Suggestion.Position(0, 6),
            Suggestion.Position(2, 10)
          )
        )
      )
    }

    "build function with associated type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo : Number -> Number
          |    foo a = a + 1
          |    foo 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        ),
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
          )
        )
      )
    }

    "build atom simple" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """type MyType a b"""
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "MyType",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          returnType    = "MyType",
          documentation = None
        )
      )
    }

    "build atom with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """## My sweet type
          |type MyType a b""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "MyType",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          returnType    = "MyType",
          documentation = Some(" My sweet type")
        )
      )
    }

    "build type simple" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type Maybe
          |    type Nothing
          |    type Just a""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "Nothing",
          arguments     = Seq(),
          returnType    = "Nothing",
          documentation = None
        ),
        Suggestion.Atom(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "Just",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType    = "Just",
          documentation = None
        )
      )
    }

    "build type with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """## When in doubt
          |type Maybe
          |    ## Nothing here
          |    type Nothing
          |    ## Something there
          |    type Just a""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "Nothing",
          arguments     = Seq(),
          returnType    = "Nothing",
          documentation = Some(" Nothing here")
        ),
        Suggestion.Atom(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "Just",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType    = "Just",
          documentation = Some(" Something there")
        )
      )
    }

    "build type with methods, type signatures and docs" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type List
          |    ## And more
          |    type Cons
          |    ## End
          |    type Nil
          |
          |    ## a method
          |    empty : List
          |    empty = Nil
          |""".stripMargin
      val module = code.preprocessModule
      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "Cons",
          arguments     = Seq(),
          returnType    = "Cons",
          documentation = Some(" And more")
        ),
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "Nil",
          arguments     = Seq(),
          returnType    = "Nil",
          documentation = Some(" End")
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "empty",
          arguments = Seq(
            Suggestion.Argument("this", "Cons", false, false, None)
          ),
          selfType      = "Cons",
          returnType    = "List",
          documentation = Some(" a method")
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "empty",
          arguments = Seq(
            Suggestion.Argument("this", "Nil", false, false, None)
          ),
          selfType      = "Nil",
          returnType    = "List",
          documentation = Some(" a method")
        )
      )
    }

    "build type with methods" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type Maybe
          |    type Nothing
          |    type Just a
          |
          |    map f = case this of
          |        Just a  -> Just (f a)
          |        Nothing -> Nothing""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "Nothing",
          arguments     = Seq(),
          returnType    = "Nothing",
          documentation = None
        ),
        Suggestion.Atom(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "Just",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType    = "Just",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "map",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("f", "Any", false, false, None)
          ),
          selfType      = "Just",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "map",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("f", "Any", false, false, None)
          ),
          selfType      = "Nothing",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build type with methods with type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type MyType
          |    type MyAtom
          |
          |    is_atom : this -> Boolean
          |    is_atom = true""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId    = None,
          module        = "Unnamed.Test",
          name          = "MyAtom",
          arguments     = Seq(),
          returnType    = "MyAtom",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "is_atom",
          arguments = Seq(
            Suggestion.Argument("this", "MyAtom", false, false, None)
          ),
          selfType      = "MyAtom",
          returnType    = "Boolean",
          documentation = None
        )
      )
    }

    "build module" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type MyType a b
          |
          |main = IO.println("Hello!")""".stripMargin
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "MyType",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          returnType    = "MyType",
          documentation = None
        ),
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build method with external id" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """main = IO.println "Hello!"
          |
          |
          |#### METADATA ####
          |[[{"index": {"value": 7}, "size": {"value": 19}}, "4083ce56-a5e5-4ecd-bf45-37ddf0b58456"]]
          |[]
          |""".stripMargin.linesIterator.mkString("\n")
      val module = code.preprocessModule

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId =
            Some(UUID.fromString("4083ce56-a5e5-4ecd-bf45-37ddf0b58456")),
          module = "Unnamed.Test",
          name   = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build function with external id" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
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

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Function(
          externalId =
            Some(UUID.fromString("f533d910-63f8-44cd-9204-a1e2d46bb7c3")),
          module = "Unnamed.Test",
          name   = "id",
          arguments = Seq(
            Suggestion.Argument("x", "Any", false, false, None)
          ),
          returnType = "Any",
          scope = Suggestion.Scope(
            Suggestion.Position(0, 6),
            Suggestion.Position(2, 28)
          )
        )
      )
    }

    "build local with external id" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
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

      build(code, module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          externalId = None,
          module     = "Unnamed.Test",
          name       = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "Test",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Local(
          externalId =
            Some(UUID.fromString("0270bcdf-26b8-4b99-8745-85b3600c7359")),
          module     = "Unnamed.Test",
          name       = "foo",
          returnType = "Any",
          scope = Suggestion.Scope(
            Suggestion.Position(0, 6),
            Suggestion.Position(2, 18)
          )
        )
      )
    }

  }

  private val Module = QualifiedName(List("Unnamed"), "Test")

  private def build(source: String, ir: IR.Module): Vector[Suggestion] =
    SuggestionBuilder(source).build(Module.toString, ir)

  private def freshModuleContext: ModuleContext =
    buildModuleContext(
      moduleName      = QualifiedName.simpleName(Module.item),
      freshNameSupply = Some(new FreshNameSupply)
    )
}
