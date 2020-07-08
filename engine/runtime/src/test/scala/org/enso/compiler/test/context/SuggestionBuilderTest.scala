package org.enso.compiler.test.context

import org.enso.compiler.Passes
import org.enso.compiler.context.{
  FreshNameSupply,
  ModuleContext,
  SuggestionBuilder
}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassManager
import org.enso.compiler.test.CompilerTest
import org.enso.polyglot.Suggestion

class SuggestionBuilderTest extends CompilerTest {

  implicit val passManager: PassManager = new Passes().passManager

  "SuggestionBuilder" should {

    "build method without explicit arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """foo = 42""".stripMargin
      val module = code.preprocessModule

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "here",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "here",
          returnType    = "Any",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          selfType      = "here",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Local("Test", "x", "Number", Suggestion.Scope(9, 62)),
        Suggestion.Local("Test", "y", "Any", Suggestion.Scope(9, 62))
      )
    }

    "build method with default arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """foo (a = 0) = a + 1""".stripMargin
      val module = code.preprocessModule

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", false, true, Some("0"))
          ),
          selfType      = "here",
          returnType    = "Any",
          documentation = None
        )
      )
    }

    "build method with associated type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """
          |MyAtom.bar : Number -> Number -> Number
          |MyAtom.bar a b = a + b
          |""".stripMargin
      val module = code.preprocessModule

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "bar",
          arguments = Seq(
            Suggestion.Argument("this", "MyAtom", false, false, None),
            Suggestion.Argument("a", "Number", false, false, None),
            Suggestion.Argument("b", "Number", false, false, None)
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
        """foo ~a = a + 1""".stripMargin
      val module = code.preprocessModule

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("a", "Any", true, false, None)
          ),
          selfType      = "here",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "here",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Function(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType = "Any",
          scope      = Suggestion.Scope(6, 35)
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Method(
          module = "Test",
          name   = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "here",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Function(
          module = "Test",
          name   = "foo",
          arguments = Seq(
            Suggestion.Argument("a", "Number", false, false, None)
          ),
          returnType = "Number",
          scope      = Suggestion.Scope(6, 62)
        )
      )
    }

    "build atom simple" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """type MyType a b"""
      val module = code.preprocessModule

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module = "Test",
          name   = "MyType",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module = "Test",
          name   = "MyType",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module        = "Test",
          name          = "Nothing",
          arguments     = Seq(),
          returnType    = "Nothing",
          documentation = None
        ),
        Suggestion.Atom(
          module = "Test",
          name   = "Just",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module        = "Test",
          name          = "Nothing",
          arguments     = Seq(),
          returnType    = "Nothing",
          documentation = Some(" Nothing here")
        ),
        Suggestion.Atom(
          module = "Test",
          name   = "Just",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType    = "Just",
          documentation = Some(" Something there")
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module        = "Test",
          name          = "Nothing",
          arguments     = Seq(),
          returnType    = "Nothing",
          documentation = None
        ),
        Suggestion.Atom(
          module = "Test",
          name   = "Just",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None)
          ),
          returnType    = "Just",
          documentation = None
        ),
        Suggestion.Method(
          module = "Test",
          name   = "map",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None),
            Suggestion.Argument("f", "Any", false, false, None)
          ),
          selfType      = "Just",
          returnType    = "Any",
          documentation = None
        ),
        Suggestion.Method(
          module = "Test",
          name   = "map",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module        = "Test",
          name          = "MyAtom",
          arguments     = Seq(),
          returnType    = "MyAtom",
          documentation = None
        ),
        Suggestion.Method(
          module = "Test",
          name   = "is_atom",
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

      build(module) should contain theSameElementsAs Seq(
        Suggestion.Atom(
          module = "Test",
          name   = "MyType",
          arguments = Seq(
            Suggestion.Argument("a", "Any", false, false, None),
            Suggestion.Argument("b", "Any", false, false, None)
          ),
          returnType    = "MyType",
          documentation = None
        ),
        Suggestion.Method(
          module = "Test",
          name   = "main",
          arguments = Seq(
            Suggestion.Argument("this", "Any", false, false, None)
          ),
          selfType      = "here",
          returnType    = "Any",
          documentation = None
        )
      )
    }
  }

  private val Module = "Test"

  private def build(ir: IR.Module): Vector[Suggestion] =
    new SuggestionBuilder().build(Module, ir)

  private def freshModuleContext: ModuleContext =
    ModuleContext(freshNameSupply = Some(new FreshNameSupply))
}
