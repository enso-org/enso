package org.enso.compiler.test.core.ir

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.test.CompilerTest
import shapeless.test.illTyped

class MetadataStorageTest extends CompilerTest {

  // === Test Utilities =======================================================

  case object TestPass1 extends IRPass {
    override type Metadata = Metadata1
    override type Config   = IRPass.Configuration.Default

    override val precursorPasses: Seq[IRPass]   = List()
    override val invalidatedPasses: Seq[IRPass] = List()

    override def runModule(
      ir: IR.Module,
      moduleContext: ModuleContext
    ): IR.Module = ir

    override def runExpression(
      ir: IR.Expression,
      inlineContext: InlineContext
    ): IR.Expression = ir

    override def updateMetadataInDuplicate[T <: IR](
      sourceIr: T,
      copyOfIr: T
    ): T = copyOfIr

    sealed case class Metadata1() extends IRPass.IRMetadata {
      override val metadataName: String = "TestPass1.Metadata1"

      override def prepareForSerialization(
        compiler: Compiler
      ): Metadata1 = this

      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[Metadata1] = Some(this)

      override def duplicate(): Option[IRPass.IRMetadata] = Some(Metadata1())
    }
  }

  case object TestPass2 extends IRPass {
    override type Metadata = Metadata2
    override type Config   = IRPass.Configuration.Default

    override val precursorPasses: Seq[IRPass]   = List()
    override val invalidatedPasses: Seq[IRPass] = List()

    override def runModule(
      ir: IR.Module,
      moduleContext: ModuleContext
    ): IR.Module = ir

    override def runExpression(
      ir: IR.Expression,
      inlineContext: InlineContext
    ): IR.Expression = ir

    override def updateMetadataInDuplicate[T <: IR](
      sourceIr: T,
      copyOfIr: T
    ): T = copyOfIr

    sealed case class Metadata2() extends IRPass.IRMetadata {
      override val metadataName: String = "TestPass2.Metadata2"

      override def prepareForSerialization(
        compiler: Compiler
      ): Metadata2 = this

      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[Metadata2] = Some(this)

      override def duplicate(): Option[IRPass.IRMetadata] = Some(Metadata2())
    }
  }

  // === The Tests ============================================================

  "The metadata storage" should {
    "allow adding metadata pairs" in {
      val meta = MetadataStorage()

      val pass     = TestPass1
      val passMeta = TestPass1.Metadata1()
      val depPair  = pass -->> passMeta

      meta.update(depPair)
      meta.get(pass) shouldEqual Some(passMeta)
    }

    "allow adding metadata" in {
      val meta = MetadataStorage()

      val meta1 = TestPass1.Metadata1()
      val meta2 = TestPass2.Metadata2()

      meta.update(TestPass1)(meta1)
      meta.update(TestPass2)(meta2)

      meta.get(TestPass1) shouldEqual Some(meta1)
      meta.get(TestPass2) shouldEqual Some(meta2)
    }

    "allow getting metadata" in {
      val meta     = MetadataStorage()
      val passMeta = TestPass1.Metadata1()

      meta.update(TestPass1)(passMeta)
      meta.get(TestPass1) shouldEqual Some(passMeta)
    }

    "allow unsafely getting metadata" in {
      val meta     = MetadataStorage()
      val passMeta = TestPass1.Metadata1()

      meta.update(TestPass1)(passMeta)
      meta.getUnsafe(TestPass1)("aaaa") shouldEqual passMeta

      def testThrow = meta.getUnsafe(TestPass2)("aaa")
      val msg       = "Compiler Internal Error: aaa"

      the[CompilerError] thrownBy testThrow should have message msg
    }

    "allow updating metadata" in {
      val meta = MetadataStorage()

      val meta1 = TestPass1.Metadata1()
      val meta2 = TestPass1.Metadata1()

      meta.update(TestPass1)(meta1)
      meta.get(TestPass1) shouldEqual Some(meta1)
      meta.update(TestPass1)(meta2)
      meta.get(TestPass1) shouldEqual Some(meta2)
    }

    "allow removing metadata" in {
      val meta = MetadataStorage()

      val meta1 = TestPass1.Metadata1()
      meta.update(TestPass1)(meta1)

      meta.remove(TestPass1) shouldEqual Some(meta1)
      meta.get(TestPass1) shouldEqual None
    }

    "compare equal when containing the same metadata" in {
      val meta1 = MetadataStorage()
      val meta2 = MetadataStorage()

      meta1 shouldEqual meta2

      meta1.update(TestPass1)(TestPass1.Metadata1())
      meta2.update(TestPass1)(TestPass1.Metadata1())

      meta1 shouldEqual meta2
    }

    "allow mapping over the internal mapping to generate some output" in {
      val meta = MetadataStorage(
        TestPass1 -->> TestPass1.Metadata1(),
        TestPass2 -->> TestPass2.Metadata2()
      )

      val expected = Map(
        TestPass1 -> "TestPass1.Metadata1",
        TestPass2 -> "TestPass2.Metadata2"
      )

      meta.map((p, m) => (p, m.metadataName)) shouldEqual expected
    }

    "allow copying to create a new instance with the same data" in {
      val meta = MetadataStorage(
        TestPass1 -->> TestPass1.Metadata1(),
        TestPass2 -->> TestPass2.Metadata2()
      )

      val expected = MetadataStorage(
        TestPass1 -->> TestPass1.Metadata1(),
        TestPass2 -->> TestPass2.Metadata2()
      )

      meta.copy shouldEqual meta
      meta.copy shouldEqual expected
    }

    "enforce safe construction" in {
      val test1 = TestPass1 -->> TestPass1.Metadata1()
      val test2 = TestPass2 -->> TestPass2.Metadata2()

      MetadataStorage(test1, test2)

      illTyped("TestPass1 -->> TestPass2.Metadata1()")
      illTyped("PassConfiguration(test1, (1, 1))")
    }
  }
}
