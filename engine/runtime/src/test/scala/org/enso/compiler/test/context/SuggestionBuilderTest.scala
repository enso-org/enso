package org.enso.compiler.test.context

import org.enso.compiler.context.SuggestionBuilder
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.Context
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
    .asHostObject[Context]()

  implicit private class PreprocessModule(code: String) {
    def preprocessModule: IR.Module = {
      val module = new runtime.Module(Module, null, code)
      langCtx.getCompiler.run(module)
      module.getIr
    }
  }
//  implicit val passManager: PassManager = new Passes(defaultConfig).passManager

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

  "SuggestionBuilder" should {

    "build method without explicit arguments" in {

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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = Some(" The foo")
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Number",
              documentation = Some(" The foo")
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Foo.Bar",
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with an argument" in {

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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
                Suggestion.Argument("a", "Text", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Number",
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
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
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Foo.Bar Baz",
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, true, Some("0"))
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
          |MyType.bar a b = a + b
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyType",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.MyType",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None),
                Suggestion
                  .Argument("b", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyAtom",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.MyAtom",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument("a", "Number", false, false, None),
                Suggestion.Argument("b", "Number", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyAtom",
              returnType    = "Number",
              documentation = Some(" My bar")
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
          |MyAtom.apply f = f self
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyAtom",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.MyAtom",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument("f", "Number -> Number", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyAtom",
              returnType    = "Number",
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build method with union type signature" in {

      val code =
        """type MyAtom
          |
          |MyAtom.apply : (Number | Text | MyAtom) -> Number
          |MyAtom.apply f = f self
          |""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "MyAtom",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.MyAtom",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyAtom", false, false, None),
                Suggestion.Argument(
                  "f",
                  "Number | Text | Unnamed.Test.MyAtom",
                  false,
                  false,
                  None
                )
              ),
              selfType      = "Unnamed.Test.MyAtom",
              returnType    = "Number",
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, true, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.A",
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
                Suggestion.Argument("a", "Unnamed.Test.A", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Unnamed.Test.A",
              documentation = None
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
            Suggestion.Atom(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "MyType",
              arguments = Seq(
                Suggestion
                  .Argument("a", SuggestionBuilder.Any, false, false, None)
              ),
              returnType    = "Unnamed.Test.MyType",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              documentation = Some(" My conversion")
            ),
            Vector()
          )
        )
      )
    }

    "build conersion method for complex type" in {
      pending
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
              returnType    = "Unnamed.Test.MyMaybe",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyMaybe", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyMaybe",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "None",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.None",
              documentation = None
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
              returnType    = "Unnamed.Test.Newtype",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.Newtype", false, false, None)
              ),
              selfType      = "Unnamed.Test.NewType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.A",
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "A",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.A",
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              returnType    = "Unnamed.Test.MyType",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build atom with documentation" in {

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
              documentation = Some(" My sweet type")
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build type simple" in {

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
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Nothing",
              documentation = None
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
              documentation = None
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
                  .Argument("self", "Unnamed.Test.Just", false, false, None)
              ),
              selfType      = "Unnamed.Test.Just",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              documentation = Some(" Nothing here")
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
              documentation = Some(" Something there")
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
                  .Argument("self", "Unnamed.Test.Just", false, false, None)
              ),
              selfType      = "Unnamed.Test.Just",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
              documentation = Some(" And more")
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
              documentation = Some(" End")
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
                  .Argument("self", "Unnamed.Test.Cons", false, false, None)
              ),
              selfType      = "Unnamed.Test.Cons",
              returnType    = "Unnamed.Test.List",
              documentation = Some(" a method")
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
                  .Argument("self", "Unnamed.Test.Nil", false, false, None)
              ),
              selfType      = "Unnamed.Test.Nil",
              returnType    = "Unnamed.Test.List",
              documentation = Some(" a method")
            ),
            Vector()
          )
        )
      )
    }

    "build type with methods, without type signatures" in {
      val code =
        """type Maybe
          |    type Nothing
          |    type Just a
          |
          |    map f = case self of
          |        Just a  -> Just (f a)
          |        Nothing -> Nothing""".stripMargin
      val module = code.preprocessModule

      build(code, module) shouldEqual Tree.Root(
        Vector(
          ModuleNode,
          Tree.Node(
            Suggestion.Atom(
              externalId    = None,
              module        = "Unnamed.Test",
              name          = "Nothing",
              arguments     = Seq(),
              returnType    = "Unnamed.Test.Nothing",
              documentation = None
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
              documentation = None
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
                  .Argument("self", "Unnamed.Test.Just", false, false, None)
              ),
              selfType      = "Unnamed.Test.Just",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                  .Argument("self", "Unnamed.Test.Nothing", false, false, None),
                Suggestion
                  .Argument("f", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test.Nothing",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                  .Argument("self", "Unnamed.Test.Just", false, false, None),
                Suggestion
                  .Argument("f", SuggestionBuilder.Any, false, false, None)
              ),
              selfType      = "Unnamed.Test.Just",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with simple atom" in {
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
              returnType    = "Unnamed.Test.MyType",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                  .Argument("self", "Unnamed.Test.MyType", false, false, None)
              ),
              selfType      = "Unnamed.Test.MyType",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with an atom named as module" in {
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
              returnType    = "Unnamed.Test.Test",
              documentation = None
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
                  .Argument("self", "Unnamed.Test.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
            ),
            Vector()
          )
        )
      )
    }

    "build module with overloaded functions" in {
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
                externalId    = None,
                module        = "Unnamed.Test",
                name          = "A",
                arguments     = List(),
                returnType    = "Unnamed.Test.A",
                documentation = None
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
                  .Argument("self", "Unnamed.Test.A", false, false, None),
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  Some(List("Unnamed.Test.A"))
                )
              ),
              selfType      = "Unnamed.Test.A",
              returnType    = "Unnamed.Test.A",
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "quux",
              arguments = Vector(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None),
                Suggestion.Argument(
                  "x",
                  "Unnamed.Test.A",
                  false,
                  false,
                  None,
                  Some(List("Unnamed.Test.A"))
                )
              ),
              selfType      = "Unnamed.Test",
              returnType    = "Unnamed.Test.A",
              documentation = None
            ),
            Vector()
          ),
          Tree.Node(
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = List(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
            Suggestion.Method(
              externalId =
                Some(UUID.fromString("4083ce56-a5e5-4ecd-bf45-37ddf0b58456")),
              module = "Unnamed.Test",
              name   = "main",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "main",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = None
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
            Suggestion.Method(
              externalId = None,
              module     = "Unnamed.Test",
              name       = "foo",
              arguments = Seq(
                Suggestion.Argument("self", "Unnamed.Test", false, false, None)
              ),
              selfType      = "Unnamed.Test",
              returnType    = SuggestionBuilder.Any,
              documentation = Some(" The foo")
            ),
            Vector()
          )
        )
      )
    }

    "provide type variants when applicable" in {
      val code =
        """type My_Tp
          |    type Variant_A
          |    type Variant_B
          |
          |foo : My_Tp -> My_Tp
          |foo arg = arg.do_sth""".stripMargin
      val module      = code.preprocessModule
      val suggestions = build(code, module)
      val fooSuggestion = suggestions.collectFirst {
        case s: Suggestion.Method if s.name == "foo" => s
      }
      val fooArg = fooSuggestion.get.arguments(1)
      fooArg.reprType shouldEqual "Unnamed.Test.My_Tp"
      fooArg.tagValues shouldEqual Some(
        List("Unnamed.Test.Variant_A", "Unnamed.Test.Variant_B")
      )
    }
  }

  private def build(source: String, ir: IR.Module): Tree.Root[Suggestion] =
    SuggestionBuilder(source).build(Module, ir)
}
