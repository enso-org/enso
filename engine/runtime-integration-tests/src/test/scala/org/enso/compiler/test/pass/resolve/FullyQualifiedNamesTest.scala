package org.enso.compiler.test.pass.resolve

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.Module
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.resolve.FullyQualifiedNames
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames}
import org.graalvm.polyglot.Value
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.mutable.ListBuffer

class FullyQualifiedNamesTest extends AnyWordSpecLike with Matchers {
  private val ctx = new InterpreterContext()
  private val langCtx = ctx.ctx
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  private val moduleName = QualifiedName(List("My_Project"), "Main")

  implicit private class PreprocessModule(code: String) {
    def preprocessModule(name: QualifiedName): Module = {
      val module = new runtime.Module(
        name,
        null,
        code.stripMargin.linesIterator.mkString("\n")
      )
      langCtx.getCompiler.run(module.asCompilerModule())
      module.getIr
    }

    def preprocessModule: Module =
      preprocessModule(moduleName)
  }

  private def collectMetadata(
    ir: Module
  ): List[FullyQualifiedNames.Metadata] = {
    ir.preorder()
      .flatMap(exp => {
        exp.passData().get(FullyQualifiedNames) match {
          case Some(passData) =>
            Some(passData.asInstanceOf[FullyQualifiedNames.Metadata])
          case None => None
        }
      })
  }

  /** Collect all the BindingsMap Resolutions from the whole IR, no matter from which
    * IR metadata pass they originate.
    * @return
    */
  private def collectResolutions(
    ir: Module
  ): List[(IR, BindingsMap.Resolution)] = {
    val buffer = ListBuffer[(IR, BindingsMap.Resolution)]()
    ir.preorder()
      .foreach(exp => {
        exp.passData.map((_, value) => {
          value match {
            case resolution: BindingsMap.Resolution =>
              buffer.addOne((exp, resolution))
            case _ => ()
          }
        })
      })
    buffer.toList
  }

  private def collectResolutionNames(
    ir: Module
  ): List[QualifiedName] = {
    collectResolutions(ir).map(_._2.target.qualifiedName)
  }

  private def execMain(
    src: String
  ): Value = {
    val mod        = ctx.executionContext.evalModule(src, moduleName.toString)
    val assocType  = mod.getAssociatedType
    val mainMethod = mod.getMethod(assocType, "main").get
    val res        = mainMethod.execute()
    res
  }

  "Fully qualified names" should {
    "be able to reference types in exported modules" in {
      val src =
        """
          |import Standard.Base
          |main =
          |    Standard.Base.Data.Vector
          |""".stripMargin
      val ir       = src.preprocessModule

      val resolutionNames = collectResolutionNames(ir)
      resolutionNames should contain
      QualifiedName(List("Standard", "Base", "Data"), "Vector")

      val res = execMain(src)
      res.isMetaObject shouldBe true
      res.getMetaSimpleName shouldBe "Vector"
    }

    "be able to reference types in synthetic modules via logical path" in {
      val src =
        """
          |import Test.Fully_Qualified_Names
          |main =
          |    Test.Fully_Qualified_Names.A_Type
          |""".stripMargin
      val ir              = src.preprocessModule
      val resolutionNames = collectResolutionNames(ir)
      resolutionNames should contain
      QualifiedName(
        List("Test", "Fully_Qualified_Names", "Synthetic_Mod", "A_Mod"),
        "A_Type"
      )

      val metadata = collectMetadata(ir)
      metadata.size shouldBe 1
      metadata.head.target
        .asInstanceOf[FullyQualifiedNames.ResolvedModule]
        .moduleRef
        .getName shouldBe (
        QualifiedName(List("Test", "Fully_Qualified_Names"), "Main")
      )

      val res = execMain(src)
      res.isMetaObject shouldBe true
      res.getMetaSimpleName shouldBe "A_Type"
    }

    "be able to reference types in synthetic modules via physical path" in {
      val src =
        """
          |import Test.Fully_Qualified_Names
          |main =
          |    Test.Fully_Qualified_Names.Synthetic_Mod.A_Mod.A_Type
          |""".stripMargin
      val ir          = src.preprocessModule
      val fqnMetadata = collectMetadata(ir)
      fqnMetadata.size shouldBe 1
      fqnMetadata.head.target
        .asInstanceOf[FullyQualifiedNames.ResolvedModule]
        .moduleRef
        .getName shouldBe (
        QualifiedName(
          List("Test", "Fully_Qualified_Names", "Synthetic_Mod"),
          "A_Mod"
        )
      )

      val resolutionNames = collectResolutionNames(ir)
      resolutionNames should contain
      QualifiedName(
        List("Test", "Fully_Qualified_Names", "Synthetic_Mod", "A_Mod"),
        "A_Type"
      )
    }

    "be able to reference methods in synthetic modules via physical path (Execution)" in {
      val src =
        """
          |import Test.Fully_Qualified_Names
          |main =
          |    Test.Fully_Qualified_Names.Synthetic_Mod.A_Mod.static_method "foo"
          |""".stripMargin
      val mod        = ctx.executionContext.evalModule(src, moduleName.toString)
      val assocType  = mod.getAssociatedType
      val mainMethod = mod.getMethod(assocType, "main").get
      val res        = mainMethod.execute()
      res.isString shouldBe true
      res.asString shouldBe "foo"
    }
  }
}
