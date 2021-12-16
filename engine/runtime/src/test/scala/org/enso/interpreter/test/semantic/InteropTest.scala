package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest, InterpreterException}

class InteropTest extends InterpreterTest {
  override def subject: String = "Interop Library"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "support tail recursive functions" in {
      val code =
        """
          |main =
          |    recurFun = i -> if i == 0 then 0 else recurFun i-1
          |    recurFun
          |""".stripMargin

      val recurFun = eval(code)
      recurFun.call(15) shouldEqual 0
    }

    "support calling curried functions" in {
      val code =
        """
          |main =
          |    fun = x -> y -> z -> x + y + z
          |    fun y=1
          |""".stripMargin

      val curriedFun = eval(code)
      curriedFun.call(2, 3) shouldEqual 6
    }

    "support creating curried calls" in {
      val code =
        """
          |main = x -> y -> z -> x + y + z
          |""".stripMargin

      val fun = eval(code)
      fun.call(1).call(2).call(3) shouldEqual 6
    }

    "work with oversaturated calls on unresolved methods returned from functions" in {
      val code =
        """from Standard.Builtins import all
          |
          |Any.method = this
          |
          |main = x -> .method
          |""".stripMargin

      val fun = eval(code)
      fun.call(1, 2) shouldEqual 2
    }

    "work with unresolved symbols" in {
      val code =
        """from Standard.Builtins import all
          |
          |Number.add x = x + this
          |Text.add x = this + x
          |
          |main = .add
          |""".stripMargin
      val symbol = eval(code)
      symbol.call(1, 2) shouldEqual 3
      symbol.call(3, 4) shouldEqual 7
      symbol.execute("Hello", " World") shouldEqual "Hello World"
    }

    "work with unresolved symbols from builtin scope" in {
      val code   = "main = .to_text"
      val symbol = eval(code)
      symbol.call(1) shouldEqual "1"
      symbol.execute("Foo") shouldEqual "'Foo'"
    }

    "successfully execute panic preprocessor used internally in IDE" in {
      // val moduleName = "Standard.Base.Main"
      val code       =
        // FIXME load from a file in /app/gui/...
        """
          |
          |x ->
          |    result = Ref.new '{ message: ""}'
          |    # If x is a PanicSentinel, rethrow it and convert to Error. If x is Error, this keeps it as such.
          |    recovered = Panic.recover (Panic.throw x)
          |    recovered.catch err->
          |        message = err.to_display_text
          |        Ref.put result ('{ "kind": "Dataflow", "message": ' + message.to_json.to_text + '}')
          |    Ref.get result
          |""".stripMargin

      interpreterContext.output.reset()
      val module = InterpreterException.rethrowPolyglot(
        // interpreterContext.executionContext.evalModule("from Standard.Builtins import all", moduleName)
        interpreterContext.executionContext.evalModule("from Standard.Builtins import all", "Test")
      )
      val preprocessor = InterpreterException.rethrowPolyglot(
        module.evalExpression(code)
      )
      val panic = the[InterpreterException] thrownBy(module.evalExpression("Panic.throw 'test-panic'"))
      // val panic = module.evalExpression("'test-panic'")
      preprocessor.execute(panic) shouldEqual "'asdf'"
    }
  }
}
