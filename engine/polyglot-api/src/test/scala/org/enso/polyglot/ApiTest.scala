package org.enso.polyglot

import org.graalvm.polyglot.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ApiTest extends AnyFlatSpec with Matchers {
  import LanguageInfo._
  val executionContext = new ExecutionContext(Context.newBuilder(ID).build())

  "Parsing a file and calling a toplevel function defined in it" should "be possible" in {
    val code =
      """
        |foo = x -> x + 1
        |bar = x -> here.foo x + 1
        |""".stripMargin
    val module                = executionContext.evalModule(code, "Test")
    val associatedConstructor = module.getAssociatedConstructor
    val barFunction           = module.getMethod(associatedConstructor, "bar")
    val result = barFunction.execute(
      associatedConstructor.newInstance(),
      10L.asInstanceOf[AnyRef]
    )
    result.asLong shouldEqual 12
  }

  "Parsing a file and calling a method on an arbitrary atom" should "be possible" in {
    val code =
      """
        |type Vector x y z
        |
        |Vector.squares = case this of
        |    Vector x y z -> Vector x*x y*y z*z
        |
        |Vector.sum = case this of
        |    Vector x y z -> x + y + z
        |
        |Vector.squareNorm = this.squares.sum
        |""".stripMargin
    val module     = executionContext.evalModule(code, "Test")
    val vectorCons = module.getConstructor("Vector")
    val squareNorm = module.getMethod(vectorCons, "squareNorm")
    val testVector = vectorCons.newInstance(
      1L.asInstanceOf[AnyRef],
      2L.asInstanceOf[AnyRef],
      3L.asInstanceOf[AnyRef]
    )
    val testVectorNorm = squareNorm.execute(testVector)
    testVectorNorm.asLong shouldEqual 14
  }

  "Creating a module and adding code to it" should "be possible" in {
    val code1 =
      """
        |bar = x -> here.foo x + 1
        |""".stripMargin
    val code2 =
      """
        |foo = x -> x + 2
        |""".stripMargin
    val topScope = executionContext.getTopScope
    val module   = topScope.createModule("Test")
    module.patch(code1)
    module.patch(code2)
    val assocCons = module.getAssociatedConstructor
    val bar       = module.getMethod(assocCons, "bar")
    val result    = bar.execute(assocCons.newInstance(), 5L.asInstanceOf[AnyRef])
    result.asLong shouldEqual 8
  }

  "Evaluating an expression in a context of a given module" should "be possible" in {
    val code =
      """
        |foo = x -> x + 2
        |""".stripMargin
    val module = executionContext.evalModule(code, "Test")
    val result = module.evalExpression("here.foo 10")
    result.asLong shouldEqual 12
  }
}
