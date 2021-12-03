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
import org.enso.docs.generator.DocParserWrapper
import org.enso.pkg.QualifiedName
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.Tree

import java.util.UUID

class SuggestionBuilderTest extends CompilerTest {

  implicit val passManager: PassManager = new Passes(defaultConfig).passManager

  private val Module = QualifiedName(List("Unnamed"), "Test")
  private val ModuleNode = Tree.Node(
    Suggestion.Module(
      module            = Module.toString,
      documentation     = None,
      documentationHtml = None,
      reexport          = None
    ),
    Vector()
  )
  private val moduleDoc = "Module doc"
  private val DoccedModuleNode = Tree.Node(
    Suggestion.Module(
      module        = Module.toString,
      documentation = Some(" " + moduleDoc),
      documentationHtml =
        Some(DocParserWrapper.runOnPureDoc(moduleDoc, Module.toString)),
      reexport = None
    ),
    Vector()
  )

  "SuggestionBuilder" should {

    "build method without explicit arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """foo = 42"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = Some(" The foo"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" The foo", "foo"))
            ),
            Vector()
          )
        )
      )
    }

    "build method with type and documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Number",
              documentation = Some(" The foo"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" The foo", "foo"))
            ),
            Vector()
          )
        )
      )
    }

    "build method with a qualified type" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """
          |foo : Foo.Bar
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = "Foo.Bar",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with an argument" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """
          |foo : Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion.Argument("a", "Text", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = "Number",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with a type containing higher kinds" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """
          |foo : Either (Vector Number) Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion.Argument(
                  "a",
                  "Either (Vector Number) Text",
                  false,
                  false,
                  None
                )
              ),
              selfType          = "Unnamed.Test",
              returnType        = "Number",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with a type containing qualified higher kinds" in {
      pending // issue #1711
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """
          |foo : Foo.Bar Baz
          |foo = 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = "Foo.Bar Baz",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with complex body" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector(
              Tree.Node(
                Suggestion.Local(
                  externalId = None,
                  "Unnamed.Test",
                  "x",
                  "Number",
                  Suggestion
                    .Scope(Suggestion.Position(0, 9), Suggestion.Position(4, 9))
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
                    .Scope(Suggestion.Position(0, 9), Suggestion.Position(4, 9))
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build method with default arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """foo (a = 0) = a + 1"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, true, Some("0"))
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "MyType",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.MyType",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "bar",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "not build method with undefined self type" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """MyAtom.bar a b = a + b"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(Vector(ModuleNode))
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "MyAtom",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.MyAtom",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "bar",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument("a", "Number", false, false, None),
                Suggestion.Argument("b", "Number", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyAtom",
              returnType    = "Number",
              documentation = Some(" My bar"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" My bar", "bar"))
            ),
            Vector()
          )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "MyAtom",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.MyAtom",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "apply",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument("f", "Number -> Number", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyAtom",
              returnType        = "Number",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with union type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type MyAtom
          |
          |MyAtom.apply : (Number | Text | MyAtom) -> Number
          |MyAtom.apply f = f this
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "MyAtom",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.MyAtom",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "apply",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument(
                  "f",
                  "Number | Text | Unnamed.Test.MyAtom",
                  false,
                  false,
                  None
                )
              ),
              selfType          = "Unnamed.Test.MyAtom",
              returnType        = "Number",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with lazy arguments" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """foo ~a = a + 1"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, true, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with resolved type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

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
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "A",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.A",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion.Argument("a", "Unnamed.Test.A", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = "Unnamed.Test.A",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build conversion method for simple type" in {
      pending
      implicit val moduleContext: ModuleContext = freshModuleContext

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
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.MyType",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
              sourceType    = "Number",
              documentation = Some(" My conversion"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" My conversion", "from"))
            ),
            Vector()
          )
        )
      )
    }

    "build conersion method for complex type" in {
      pending
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type MyMaybe
          |    type Some a
          |    type None
          |
          |type Newtype x
          |
          |## My conversion method
          |Newtype.from : MyMaybe -> Newtype
          |Newtype.from opt = case opt of
          |    Some a -> Newtype a
          |    None -> Newtype 0
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Some",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.MyMaybe",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyMaybe", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyMaybe",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "None",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.None",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Newtype",
              arguments = Seq(
                Suggestion
                  .Argument("x", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.Newtype",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "x",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.Newtype", false, false, None)
              ),
              selfType          = "Unnamed.Test.NewType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
              sourceType    = "Unnamed.Test.MyMaybe",
              documentation = Some(" My conversion method"),
              documentationHtml = Some(
                DocParserWrapper.runOnPureDoc(" My conversion method", "from")
              )
            ),
            Vector()
          )
        )
      )
    }

    "build function simple" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo a = a + 1
          |    foo 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build function with complex body" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo a =
          |        b = a + 1
          |        b
          |    foo 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
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
                      )
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
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo : Number -> Number
          |    foo a = a + 1
          |    foo 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build function with resolved type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type A
          |
          |main =
          |    foo : A -> A
          |    foo a = a + 1
          |    foo 42""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "A",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.A",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector(
              Tree.Node(
                Suggestion.Function(
                  externalId = None,
                  module     = "Unnamed.Test",
                  name       = "foo",
                  arguments = Seq(
                    Suggestion
                      .Argument("a", "Unnamed.Test.A", false, false, None)
                  ),
                  returnType = "Unnamed.Test.A",
                  scope = Suggestion.Scope(
                    Suggestion.Position(2, 6),
                    Suggestion.Position(5, 10)
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local simple" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo = 42
          |    foo""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local with complex body" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo =
          |        b = 42
          |        b
          |    foo""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
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
                      )
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
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    foo : Number
          |    foo = 42
          |    foo""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build local with resolved type signature" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """type A
          |
          |main =
          |    foo : A
          |    foo = A
          |    foo""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "A",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.A",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build atom simple" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code   = """type MyType a b"""
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.MyType",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build atom with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """## Module doc
          |
          |## My sweet type
          |type MyType a b""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              documentation = Some(" My sweet type"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" My sweet type", "MyType"))
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "Nothing",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.Nothing",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.Just",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.Just", false, false, None)
              ),
              selfType          = "Unnamed.Test.Just",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build type with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """## Module doc
          |
          |## When in doubt
          |type Maybe
          |    ## Nothing here
          |    type Nothing
          |    ## Something there
          |    type Just a""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          DoccedModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Nothing",
              documentation = Some(" Nothing here"),
              documentationHtml = Some(
                DocParserWrapper.runOnPureDoc(" Nothing here", "Nothing")
              )
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.Just",
              documentation = Some(" Something there"),
              documentationHtml = Some(
                DocParserWrapper.runOnPureDoc(" Something there", "Just")
              )
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.Just", false, false, None)
              ),
              selfType          = "Unnamed.Test.Just",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Cons",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Cons",
              documentation = Some(" And more"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" And more", "Cons"))
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nil",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Nil",
              documentation = Some(" End"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" End", "Nil"))
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "empty",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.Cons", false, false, None)
              ),
              selfType      = "Unnamed.Test.Cons",
              returnType    = "List",
              documentation = Some(" a method"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" a method", "empty"))
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "empty",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.Nil", false, false, None)
              ),
              selfType      = "Unnamed.Test.Nil",
              returnType    = "List",
              documentation = Some(" a method"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" a method", "empty"))
            ),
            Vector()
          )
        )
      )
    }

    "build type with methods, without type signatures" in {
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId        = None,
              module            = "Unnamed.Test",
              name              = "Nothing",
              arguments         = Seq(),
              returnType        = "Unnamed.Test.Nothing",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Just",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.Just",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.Just", false, false, None)
              ),
              selfType          = "Unnamed.Test.Just",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "map",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.Nothing", false, false, None),
                Suggestion
                  .Argument("f", SuggestionBuilder.Any, false, false, None)
              ),
              selfType          = "Unnamed.Test.Nothing",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "map",
              arguments = Seq(
                Suggestion
                  .Argument("this", "Unnamed.Test.Just", false, false, None),
                Suggestion
                  .Argument("f", SuggestionBuilder.Any, false, false, None)
              ),
              selfType          = "Unnamed.Test.Just",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with simple atom" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type MyType a b
          |
          |main = IO.println "Hello!"""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.MyType",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "b",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType          = "Unnamed.Test.MyType",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with an atom named as module" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type Test a
          |
          |main = IO.println "Hello!"""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "Test",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType        = "Unnamed.Test.Test",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "a",
              arguments = List(
                Suggestion
                  .Argument("this", "Unnamed.Test.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with overloaded functions" in {
      implicit val moduleContext: ModuleContext = freshModuleContext
      val code =
        """type A
          |    type A
          |    quux : A -> A
          |    quux x = x
          |
          |quux : A -> A
          |quux x = x
          |
          |main =
          |    here.quux A
          |    A.quux A""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion
              .Atom(
                externalId        = None,
                module            = "Unnamed.Test",
                name              = "A",
                arguments         = List(),
                returnType        = "Unnamed.Test.A",
                documentation     = None,
                documentationHtml = None
              ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "quux",
              arguments = Vector(
                Suggestion
                  .Argument("this", "Unnamed.Test.A", false, false, None),
                Suggestion.Argument("x", "Unnamed.Test.A", false, false, None)
              ),
              selfType          = "Unnamed.Test.A",
              returnType        = "Unnamed.Test.A",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "quux",
              arguments = Vector(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None),
                Suggestion.Argument("x", "Unnamed.Test.A", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = "Unnamed.Test.A",
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = List(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId =
                Some(UUID.fromString("4083ce56-a5e5-4ecd-bf45-37ddf0b58456")),
              module = "Unnamed.Test",
              name   = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
            ),
            Vector()
          )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
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

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType          = "Unnamed.Test",
              returnType        = SuggestionBuilder.Any,
              documentation     = None,
              documentationHtml = None
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
                  )
                ),
                Vector()
              )
            )
          )
        )
      )
    }

    "build module with documentation" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

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
              "Unnamed.Test",
              Some(" Module doc"),
              Some(
                DocParserWrapper.runOnPureDoc(" Module doc", "Unnamed.Test")
              ),
              None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("this", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = Some(" The foo"),
              documentationHtml =
                Some(DocParserWrapper.runOnPureDoc(" The foo", "foo"))
            ),
            Vector()
          )
        )
      )
    }
  }

  private def build(source: String, ir: IR.Module): Tree.Root[Suggestion] =
    SuggestionBuilder(source).build(Module, ir)

  private def freshModuleContext: ModuleContext =
    buildModuleContext(
      moduleName      = Module,
      freshNameSupply = Some(new FreshNameSupply)
    )
}
