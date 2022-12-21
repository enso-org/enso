package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.graalvm.polyglot.Context

// This test conceivably be written in pure Enso, but it requires a special way
// of spawning the engine, hence it's easier to keep here.
class DevelopmentContextTest extends InterpreterTest {

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(_.option("enso.IOEnvironment", "development"))

  override def subject: String = "development IO Context"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "error on Input actions" in {
      val code =
        """from Standard.Base import all
          |
          |input_action : Integer -> Integer in Input
          |input_action i = i
          |
          |main = Panic.catch Any (input_action 1) p-> p.payload.to_text
          |""".stripMargin
      eval(code) shouldEqual "(Forbidden_Operation_Data 'Input')"
    }

    "error on Output actions" in {
      val code =
        """from Standard.Base import all
          |
          |output_action : Integer -> Nothing in Output
          |output_action i = i
          |
          |main = Panic.catch Any (output_action 1) p-> p.payload.to_text
          |""".stripMargin
      eval(code) shouldEqual "(Forbidden_Operation_Data 'Output')"
    }

    "allow locally running IO" in {
      val code =
        """from Standard.Base import all
          |
          |output_action : Integer -> Integer in Output
          |output_action i = i + 1
          |
          |input_action : Integer -> Integer in Input
          |input_action i = i * 2
          |
          |main =
          |    r_1 = Runtime.allow_input_in "development" <| input_action 123
          |    r_2 = Runtime.allow_output_in "development" <| output_action 123
          |    r_3 = Runtime.allow_input_in "development" <| Runtime.allow_output_in "development" <| output_action <| input_action 123
          |    [r_1, r_2, r_3].to_text
          |""".stripMargin
      eval(code) shouldEqual "[246, 124, 247]"
    }
  }
}

class ProductionContextTest extends InterpreterTest {
  override def subject: String = "production IO Context"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "allow all IO" in {
      val code =
        """from Standard.Base import all
          |
          |output_action : Integer -> Integer in Output
          |output_action i = i + 1
          |
          |input_action : Integer -> Integer in Input
          |input_action i = i * 2
          |
          |main =
          |    r_1 = input_action 123
          |    r_2 = output_action 123
          |    r_3 = output_action <| input_action 123
          |    [r_1, r_2, r_3].to_text
          |""".stripMargin
      eval(code) shouldEqual "[246, 124, 247]"
    }
  }
}
