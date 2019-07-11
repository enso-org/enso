package org.enso.interpreter

import org.enso.interpreter.builder.VariableRedefinitionException
import org.graalvm.polyglot.PolyglotException

class LexicalScopeTest extends LanguageTest {
  "Scope capture from outer scope" should "work" in {
    val code =
      """
        |@{
        |  x = 10;
        |  @{
        |    y = x;
        |    y + 5
        |  }
        |}  
      """.stripMargin

    eval(code) shouldEqual 15
  }

  "Variable shadowing" should "work" in {
    val code =
      """
        |@{
        |  x = 10;
        |  @{
        |    x = 5;
        |    x + 1
        |  }
        |}
      """.stripMargin
    eval(code) shouldEqual 6
  }

  "Variable redefinition in same scope" should "throw error" in {
    val code =
      """
        |@{ 
        |  x = 10;
        |  @{
        |    y = x;
        |    y = 5;
        |    y + 1
        |  }
        |}
      """.stripMargin
    the[PolyglotException] thrownBy eval(code) should have message "Variable y was already defined in this scope."
  }

  "Reference to an undefined variable" should "throw error" in {
    val code =
      """
        |@{
        |  x = 10;
        |  y
        |}
      """.stripMargin
    the[PolyglotException] thrownBy eval(code) should have message "Variable y is not defined."
  }

}
