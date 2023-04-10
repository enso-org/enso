package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.graalvm.polyglot.Context

class DesignExecutionEnvironmentTest extends InterpreterTest {

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(_.option("enso.ExecutionEnvironment", "design"))

  override def subject: String = "design execution environment"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "error on Input actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i environment="design"
          |
          |main = Panic.catch Any (input_action 2) p-> p.payload.to_text
          |""".stripMargin
      eval(code) shouldEqual "(Forbidden_Operation.Error 'Input')"
    }

    "error on invalid context actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input,Output
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i environment="design"
          |
          |main = Panic.catch Any (Runtime.with_enabled_context Output (input_action 2)) p-> p.payload.to_text
          |""".stripMargin
      eval(code) shouldEqual "(Forbidden_Operation.Error 'Input')"
    }

    "error on invalid environment actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i environment="live"
          |
          |main = Panic.catch Any (input_action 2) p-> p.payload.to_text
          |""".stripMargin
      eval(
        code
      ) shouldEqual "(Unimplemented.Error execution environment mismatch)"
    }

    "pass on Input actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i environment="design"
          |
          |main = Runtime.with_enabled_context Input (input_action 2)
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "error on Output actions" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Output
          |
          |output_action : Integer -> Integer
          |output_action i = Output.if_enabled i environment="design"
          |
          |main = Panic.catch Any (output_action 2) p-> p.payload.to_text
          |""".stripMargin
      eval(code) shouldEqual "(Forbidden_Operation.Error 'Output')"
    }

    "scope of context is limited" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled i
          |
          |main =
          |    res = Runtime.with_enabled_context Input (input_action 2)
          |    Panic.catch Any (input_action 2) p-> res.to_text+" and "+p.payload.to_text
          |""".stripMargin
      eval(code) shouldEqual "2 and (Forbidden_Operation.Error 'Input')"
    }

    "allow locally running IO" in {
      val code =
        """from Standard.Base import all
          |from Standard.Base.Runtime.Context import Input, Output
          |
          |output_action : Integer -> Integer
          |output_action i = Output.if_enabled (i+1) environment="design"
          |
          |input_action : Integer -> Integer
          |input_action i = Input.if_enabled (i*2) environment="design"
          |
          |main =
          |    r = Runtime.with_enabled_context Input environment="design" <| Runtime.with_enabled_context Output environment="design" <| output_action <| input_action 123
          |    [r].to_text
          |""".stripMargin
      eval(code) shouldEqual "[247]"
    }
  }
}
