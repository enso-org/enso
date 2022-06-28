package org.enso.polyglot

import org.graalvm.polyglot.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class ApiTest extends AnyFlatSpec with Matchers {
  import LanguageInfo._
  val executionContext = new PolyglotContext(
    Context
      .newBuilder(ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile.getAbsolutePath
      )
      .build()
  )

  "Parsing a file and calling a toplevel function defined in it" should "be possible" in {
    val code =
      """
        |foo = x -> x + 1
        |bar = x -> here.foo x + 1
        |""".stripMargin
    val module                = executionContext.evalModule(code, "Test")
    val associatedConstructor = module.getAssociatedConstructor
    val barFunction           = module.getMethod(associatedConstructor, "bar").get
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
        |Vector.squares = case self of
        |    Vector x y z -> Vector x*x y*y z*z
        |
        |Vector.sum = case self of
        |    Vector x y z -> x + y + z
        |
        |Vector.squareNorm = self.squares.sum
        |""".stripMargin
    val module     = executionContext.evalModule(code, "Test")
    val vectorCons = module.getConstructor("Vector")
    val squareNorm = module.getMethod(vectorCons, "squareNorm").get
    val testVector = vectorCons.newInstance(
      1L.asInstanceOf[AnyRef],
      2L.asInstanceOf[AnyRef],
      3L.asInstanceOf[AnyRef]
    )
    val testVectorNorm = squareNorm.execute(testVector)
    testVectorNorm.asLong shouldEqual 14
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
