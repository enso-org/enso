package org.enso.runtime.parser.processor.test

import org.enso.compiler.core.ir.{Literal, MetadataStorage}
import org.enso.runtime.parser.processor.test.gen.ir.core.JCallArgument.JSpecified
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests IR elements generated from package [[org.enso.runtime.parser.processor.test.gen.ir]].
  */
class GeneratedIRTest extends AnyFlatSpec with Matchers {
  "JSpecifiedGen" should "be duplicated correctly" in {
    val lit     = Literal.Text("foo", null, new MetadataStorage())
    val callArg = new JSpecified(true, None, lit)
    callArg should not be null

    val dupl = callArg.duplicate(false, false, false, false)
    dupl.value() shouldEqual lit
  }

  "JSpecifiedGen" should "have generated parameter names with javac compiler" in {
    val lit     = Literal.Text("foo", null, new MetadataStorage())
    val callArg = new JSpecified(isSynthetic = true, value = lit, name = None)
    callArg should not be null
  }

  "JSpecifiedGen" should "have overridden toString method" in {
    val lit     = Literal.Text("foo", null, new MetadataStorage())
    val callArg = new JSpecified(true, None, lit)
    val str     = callArg.toString
    withClue(s"String representation: " + str) {
      str.contains("JCallArgument.JSpecified") shouldBe true
      str.contains("name = None") shouldBe true
      str.contains("value = Literal.Text") shouldBe true
      str.contains("location = null") shouldBe true
    }
  }
}
