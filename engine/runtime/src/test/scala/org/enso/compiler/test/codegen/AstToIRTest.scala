package org.enso.compiler.test.codegen

import org.enso.compiler.core.IR
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Debug

class AstToIRTest extends CompilerTest {

  "AST translation of lambda definitions" should {
    "result in a syntax error when defined with multiple arguments" in {
      val ir =
        """x y -> x + y
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Error.Syntax]

      ir.asInstanceOf[IR.Error.Syntax].message shouldEqual
      "Syntax is not supported yet: pattern matching function arguments."
    }

    "support standard lambda chaining" in {
      val ir =
        """
          |x -> y -> z -> x
          |""".stripMargin.toIrExpression.get

      ir shouldBe an[IR.Function.Lambda]
      ir.asInstanceOf[IR.Function.Lambda].body shouldBe an[IR.Function.Lambda]
      ir.asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body shouldBe an[IR.Function.Lambda]
    }
  }
}
