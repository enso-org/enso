package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.graalvm.polyglot.Context

class LiveExecutionEnvironmentTest extends InterpreterTest {

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(_.option("enso.ExecutionEnvironment", "live"))

  override def subject: String = "live execution environment"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "pass on Input actions for live environment" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i
          |
          |main = input_action 2
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "error on invalid context actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input,Output
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i
          |
          |main = Panic.catch Any (Runtime.with_disabled_context Input action=(input_action 2)) p-> p.payload.to_text
          |""".stripMargin
      eval(
        code
      ) shouldEqual "(Forbidden_Operation.Error 'The Input context is disabled.')"
    }

    "pass on Input actions with Context enabled explicitly" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i
          |
          |main = Runtime.with_enabled_context Input action=(input_action 2)
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "pass on Output actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Output
          |
          |output_action : Integer -> Integer
          |output_action i = Output.if_enabled i
          |
          |main = output_action 2
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "allow locally running IO" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input, Output
          |
          |output_action : Integer -> Integer
          |output_action i = Output.if_enabled (i+1)
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled (i*2)
          |
          |main =
          |    r = Runtime.with_enabled_context Input <| Runtime.with_enabled_context Output <| output_action <| input_action 123
          |    [r].to_text
          |""".stripMargin
      eval(code) shouldEqual "[247]"
    }
  }
}
