package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class CompileErrorsTest extends InterpreterTest {
  "ast-processing errors" should "be surfaced in the language" in {
    val code =
      """
        |main =
        |    x = Panic.recover ()
        |    x.catch err->
        |        case err of
        |            Syntax_Error msg -> "Oopsie, it's a syntax error: " + msg
        |""".stripMargin
    eval(code) shouldEqual "Oopsie, it's a syntax error: Parentheses can't be empty."
  }

  "parsing errors" should "be surfaced in the language" in {
    val code =
      """
        |main =
        |    x = Panic.recover @
        |    x.catch to_text
        |""".stripMargin
    eval(code) shouldEqual "Syntax_Error Unrecognized token."
  }

  "redefinition errors" should "be surfaced in the language" in {
    val code =
      """
        |foo =
        |    x = 1
        |    x = 2
        |
        |main = Panic.recover here.foo . catch to_text
        |""".stripMargin
    eval(code) shouldEqual "Compile_Error Variable x is being redefined."
  }

}
