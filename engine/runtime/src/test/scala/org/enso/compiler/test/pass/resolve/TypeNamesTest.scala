package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap.ResolutionNotFound
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.resolve.{TypeNames, TypeSignatures}
import org.enso.compiler.test.CompilerTest

class TypeNamesTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(TypeNames).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to a module for performing type signature
    * resolution.
    *
    * @param ir the IR to add the extension method to
    */
  implicit class ResolveModule(ir: IR.Module) {

    /** Resolves type signatures in [[ir]].
      *
      * @param moduleContext the context in which resolution is taking place
      * @return [[ir]], with all type signatures resolved
      */
    def resolve(implicit moduleContext: ModuleContext): IR.Module = {
      TypeNames.runModule(ir, moduleContext)
    }
  }

  /** Adds an extension method to an expression for performing type signature
    * resolution.
    *
    * @param ir the expression to add the extension method to
    */
  implicit class ResolveExpression(ir: IR.Expression) {

    /** Resolves type signatures in [[ir]].
      *
      * @param inlineContext the context in which resolution is taking place
      * @return [[ir]], with all type signatures resolved
      */
    def resolve(implicit inlineContext: InlineContext): IR.Expression = {
      TypeNames.runExpression(ir, inlineContext)
    }
  }

  /** Creates a defaulted module context.
    *
    * @return a defaulted module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  /** Creates a defaulted inline context.
    *
    * @return a defaulted inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Resolution of type names in modules" should {
    implicit val ctx: ModuleContext = mkModuleContext

    "should correctly resolve local type names" in {
      val ir =
        """
          |type A
          |type B
          |type C
          |
          |foo : A -> B -> C
          |foo a b = a + b
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 4
      val meta = ir.bindings.last.getMetadata(TypeSignatures)
      meta shouldBe defined
      val diagnostics = meta.get.signature.preorder
        .collect({ case err: IR.Diagnostic =>
          err
        })

      diagnostics shouldBe Nil
    }

    "should report failures when type name is unknown" in {
      val ir =
        """
          |type A
          |type B
          |
          |foo : A -> B -> C
          |foo a b = a + b
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 3
      val meta = ir.bindings.last.getMetadata(TypeSignatures)
      meta shouldBe defined
      val diagnostics = meta.get.signature.preorder
        .collect({ case err: IR.Diagnostic =>
          err
        })

      diagnostics.length shouldEqual 1
      val resolutionFailure = diagnostics.head.asInstanceOf[IR.Error.Resolution]
      resolutionFailure.name shouldBe "C"
      resolutionFailure.reason shouldBe IR.Error.Resolution.ResolverError(
        ResolutionNotFound
      )
    }

    "should report every failed name resolution" in {
      val ir =
        """
          |foo : Integer -> Integer -> Integer
          |foo a b = a + b
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      val meta = ir.bindings.last.getMetadata(TypeSignatures)
      meta shouldBe defined
      val diagnostics = meta.get.signature.preorder
        .collect({ case err: IR.Diagnostic =>
          err
        })

      diagnostics.length shouldEqual 3
      diagnostics.foreach {
        case d: IR.Error.Resolution =>
          d.name shouldBe "Integer"
          d.reason shouldBe IR.Error.Resolution.ResolverError(
            ResolutionNotFound
          )
        case _ =>
          fail()
      }
    }

    "should treat Suspended type specially" in {
      val ir =
        """
          |type A
          |
          |foo : A -> Suspended -> A
          |foo a b = a + b
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 2
      val meta = ir.bindings.last.getMetadata(TypeSignatures)
      meta shouldBe defined
      val diagnostics = meta.get.signature.preorder
        .collect({ case err: IR.Diagnostic =>
          err
        })

      diagnostics.length shouldEqual 0
    }
  }
}
