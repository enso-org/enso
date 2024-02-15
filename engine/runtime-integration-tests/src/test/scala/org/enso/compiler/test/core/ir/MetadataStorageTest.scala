package org.enso.compiler.test.core.ir

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.MetadataStorage
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.IRPass
import org.enso.compiler.test.CompilerTest
import shapeless.test.illTyped
import scala.jdk.CollectionConverters._

class MetadataStorageTest extends CompilerTest {

  // === Test Utilities =======================================================

  case object TestPass1 extends IRPass {
    override type Metadata = Metadata1
    override type Config   = IRPass.Configuration.Default

    override lazy val precursorPasses: Seq[IRPass]   = List()
    override lazy val invalidatedPasses: Seq[IRPass] = List()

    override def runModule(
      ir: Module,
      moduleContext: ModuleContext
    ): Module = ir

    override def runExpression(
      ir: Expression,
      inlineContext: InlineContext
    ): Expression = ir

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

    override lazy val precursorPasses: Seq[IRPass]   = List()
    override lazy val invalidatedPasses: Seq[IRPass] = List()

    override def runModule(
      ir: Module,
      moduleContext: ModuleContext
    ): Module = ir

    override def runExpression(
      ir: Expression,
      inlineContext: InlineContext
    ): Expression = ir

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
      val meta = new MetadataStorage()

      val pass     = TestPass1
      val passMeta = TestPass1.Metadata1()
      val depPair  = new MetadataPair(pass, passMeta)

      meta.update(depPair)
      meta.get(pass) shouldEqual Some(passMeta)
    }

    "allow adding metadata" in {
      val meta = new MetadataStorage()

      val meta1 = TestPass1.Metadata1()
      val meta2 = TestPass2.Metadata2()

      meta.update(TestPass1, meta1)
      meta.update(TestPass2, meta2)

      meta.get(TestPass1) shouldEqual Some(meta1)
      meta.get(TestPass2) shouldEqual Some(meta2)
    }

    "allow getting metadata" in {
      val meta     = new MetadataStorage()
      val passMeta = TestPass1.Metadata1()

      meta.update(TestPass1, passMeta)
      meta.get(TestPass1) shouldEqual Some(passMeta)
    }

    "allow unsafely getting metadata" in {
      val meta     = new MetadataStorage()
      val passMeta = TestPass1.Metadata1()

      meta.update(TestPass1, passMeta)
      meta.get(TestPass1).get shouldEqual passMeta

      meta.get(TestPass2) match {
        case None =>
        case any  => fail("Expecting None: " + any)
      }

    }

    "allow updating metadata" in {
      val meta = new MetadataStorage()

      val meta1 = TestPass1.Metadata1()
      val meta2 = TestPass1.Metadata1()

      meta.update(TestPass1, meta1)
      meta.get(TestPass1) shouldEqual Some(meta1)
      meta.update(TestPass1, meta2)
      meta.get(TestPass1) shouldEqual Some(meta2)
    }

    "allow removing metadata" in {
      val meta = new MetadataStorage()

      val meta1 = TestPass1.Metadata1()
      meta.update(TestPass1, meta1)

      meta.remove(TestPass1) shouldEqual Some(meta1)
      meta.get(TestPass1) shouldEqual None
    }

    "compare equal when containing the same metadata" in {
      val meta1 = new MetadataStorage()
      val meta2 = new MetadataStorage()

      meta1 shouldEqual meta2

      meta1.update(TestPass1, TestPass1.Metadata1())
      meta2.update(TestPass1, TestPass1.Metadata1())

      meta1 shouldEqual meta2
    }

    def newMetadataStorage(init: Seq[MetadataPair[_]]): MetadataStorage = {
      val meta = new MetadataStorage()
      for (p <- init) {
        meta.update(p)
      }
      meta
    }

    "allow mapping over the internal mapping to generate some output" in {
      val meta = newMetadataStorage(
        Seq(
          new MetadataPair(TestPass1, TestPass1.Metadata1()),
          new MetadataPair(TestPass2, TestPass2.Metadata2())
        )
      )

      val expected = List(
        (TestPass1, "TestPass1.Metadata1"),
        (TestPass2, "TestPass2.Metadata2")
      )

      meta.map((p, m) => (p, m.metadataName)).asScala shouldEqual expected
    }

    "allow copying to create a new instance with the same data" in {
      val meta = newMetadataStorage(
        Seq(
          new MetadataPair(TestPass1, TestPass1.Metadata1()),
          new MetadataPair(TestPass2, TestPass2.Metadata2())
        )
      )

      val expected = newMetadataStorage(
        Seq(
          new MetadataPair(TestPass1, TestPass1.Metadata1()),
          new MetadataPair(TestPass2, TestPass2.Metadata2())
        )
      )

      meta.duplicate shouldEqual meta
      meta.duplicate shouldEqual expected
    }

    "enforce safe construction" in {
      val test1 = new MetadataPair(TestPass1, TestPass1.Metadata1())
      val test2 = new MetadataPair(TestPass2, TestPass2.Metadata2())

      newMetadataStorage(Seq(test1, test2))

      illTyped("TestPass1 -->> TestPass2.Metadata1()")
      illTyped("PassConfiguration(test1, (1, 1))")
    }
  }
}
