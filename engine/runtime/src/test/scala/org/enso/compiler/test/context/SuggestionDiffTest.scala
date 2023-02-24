package org.enso.compiler.test.context

import org.enso.compiler.context.SuggestionBuilder
import org.enso.compiler.context.SuggestionDiff
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames, Suggestion}
import org.enso.polyglot.data.Tree
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SuggestionDiffTest extends AnyWordSpecLike with Matchers {
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

  @annotation.nowarn
  def endOfLine(line: Int, character: Int): Suggestion.Position =
    Suggestion.Position(line + 1, 0)

  "build method without explicit arguments" in {
    val ssm = new org.enso.compiler.SuggestionsSerializationManager(null)

    val expected = Tree.Root(
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
            isStatic      = true,
            documentation = None
          ),
          Vector()
        )
      )
    )

    val code   = """foo = 42"""
    val module = code.preprocessModule
    val t1 = build(code, module)

    t1 shouldEqual expected

    val data = t1.toVector
    val json = ssm.toJson(data)

    System.out.println(json)

    val t2 = SuggestionDiff.deserialize(json);
    t2 shouldEqual expected
  }
  private def build(
    source: String,
    ir: IR.Module,
    module: QualifiedName = Module
  ): Tree.Root[Suggestion] =
    SuggestionBuilder(source).build(module, ir)
}
