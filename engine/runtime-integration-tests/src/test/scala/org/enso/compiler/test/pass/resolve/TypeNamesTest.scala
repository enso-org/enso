package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.pass.MiniIRPass
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{Diagnostic, Expression, Module}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.data.BindingsMap.ResolutionNotFound
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.resolve.{TypeNames, TypeSignatures}
import org.enso.compiler.test.CompilerTest
import org.enso.compiler.dump.service.IRDumpFactoryService
import org.enso.compiler.dump.service.IRSource

class TypeNamesTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(TypeNames.INSTANCE).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to a module for performing type signature
    * resolution.
    *
    * @param ir the IR to add the extension method to
    */
  implicit class ResolveModule(ir: Module) {

    /** Resolves type signatures in [[ir]].
      *
      * @param moduleContext the context in which resolution is taking place
      * @return [[ir]], with all type signatures resolved
      */
    def resolve(implicit moduleContext: ModuleContext): Module = {
      val orig = TypeNamesOrig.runModule(ir.duplicate(), moduleContext)

      val mini = TypeNames.INSTANCE.createForModuleCompilation(moduleContext)
      val now  = MiniIRPass.compile(classOf[Module], ir, mini)

      if (orig != now) {
        val modName = moduleContext.getName().toString()
        val dumper  = IRDumpFactoryService.DEFAULT.create(modName)
        val origSrc = new IRSource(
          orig,
          modName,
          "Original",
          null,
          null
        );
        dumper.dumpModule(origSrc)
        val newSrc = new IRSource(
          now,
          modName,
          "New",
          null,
          null
        );
        dumper.dumpModule(newSrc)
        throw new IllegalStateException()
      }
      return now
    }
  }

  /** Adds an extension method to an expression for performing type signature
    * resolution.
    *
    * @param ir the expression to add the extension method to
    */
  implicit class ResolveExpression(ir: Expression) {

    /** Resolves type signatures in [[ir]].
      *
      * @param inlineContext the context in which resolution is taking place
      * @return [[ir]], with all type signatures resolved
      */
    def resolve(implicit inlineContext: InlineContext): Expression = {
      val mini = TypeNames.INSTANCE.createForInlineCompilation(inlineContext)
      if (mini != null) {
        MiniIRPass.compile(classOf[Expression], ir, mini)
      } else {
        ir
      }
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
        .collect({ case err: Diagnostic =>
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
        .collect({ case err: Diagnostic =>
          err
        })

      diagnostics.length shouldEqual 1
      val resolutionFailure = diagnostics.head.asInstanceOf[errors.Resolution]
      resolutionFailure.name shouldBe "C"
      resolutionFailure.reason shouldBe errors.Resolution.ResolverError(
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
        .collect({ case err: Diagnostic =>
          err
        })

      diagnostics.length shouldEqual 3
      diagnostics.foreach {
        case d: errors.Resolution =>
          d.name shouldBe "Integer"
          d.reason shouldBe errors.Resolution.ResolverError(
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
        .collect({ case err: Diagnostic =>
          err
        })

      diagnostics.length shouldEqual 0
    }
  }
}
