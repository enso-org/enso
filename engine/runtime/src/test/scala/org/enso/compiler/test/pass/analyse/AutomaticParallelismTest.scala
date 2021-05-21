package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.{AliasAnalysis, AutomaticParallelism}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

import scala.annotation.unused

class AutomaticParallelismTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(CompilerConfig(autoParallelismEnabled = true))

  val precursorPasses: PassGroup =
    passes.getPrecursors(AutomaticParallelism).get

  val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis         -->> AliasAnalysis.Configuration(),
    ApplicationSaturation -->> ApplicationSaturation.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  implicit class AnalyseModule(ir: IR.Module) {
    def analyse(implicit ctx: ModuleContext): IR.Module =
      AutomaticParallelism.runModule(ir, ctx)
  }

  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Successful parallelism analysis" should {
    implicit val moduleContext: ModuleContext = mkModuleContext

    @unused val code =
      """
        |fn f g =
        |    x = File.read "foo"
        |    y = File.read "bar"
        |    a = x.length
        |    b = y.length
        |
        |    @Auto_Parallel a.n b
        |""".stripMargin.preprocessModule.analyse

    "determine the separated flows" in {
      pending
    }

    "inline the flows" in {
      pending
    }

    "associate correct metadata with the block" in {
      pending
    }

    "work in nested blocks" in {
      pending
    }

    "leave unrelated things untouched" in {
      pending
    }

    "work when the result of the parallel call is later used" in {
      pending
    }

    "work with named argument applications" in {
      pending
    }
  }

  "Failed parallelism analysis" should {
    pending

    "raise a warning when there aren't enough arguments to parallelize" in {
      implicit val moduleContext: ModuleContext = mkModuleContext

      val ir =
        """
          |fn f g =
          |    x = File.read "x"
          |    @Auto_Parallel x.foo
          |""".stripMargin.preprocessModule.analyse

      val defn = ir.bindings.head.asInstanceOf[Method.Explicit]
      val parApp = defn.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Application.Prefix]
      val diag = parApp.diagnostics.collect {
        case d: IR.Warning.FailedParallelism => d
      }
      diag.length shouldEqual 1
      val expected = "Cannot parallelize an application with 1 arguments."
      diag.head.reason shouldEqual expected
    }

    "raise a warning when the incoming flows are not distinct" in {
      implicit val moduleContext: ModuleContext = mkModuleContext

      val ir =
        """
          |fn f g =
          |    x = File.read "x"
          |    y = File.read "y"
          |    a = x.length
          |    b = y + a
          |    @Auto_Parallel Table.foo a b
          |""".stripMargin.preprocessModule.analyse

      val defn = ir.bindings.head.asInstanceOf[Method.Explicit]
      val parApp = defn.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Application.Prefix]
      val diag = parApp.diagnostics.collect {
        case d: IR.Warning.FailedParallelism => d
      }
      diag.length shouldEqual 1
      val expected =
        "Arguments to the parallel call are not distinct computations."
      diag.head.reason shouldEqual expected
    }

    "raise a warning when the parallel call depends on input arguments" in {
      implicit val moduleContext: ModuleContext = mkModuleContext

      val ir =
        """
          |fn f g =
          |    x = File.read "x"
          |    y = File.read "y"
          |    a = f x
          |    b = y.length
          |    @Auto_Parallel Table.foo a b
          |""".stripMargin.preprocessModule.analyse

      val defn = ir.bindings.head.asInstanceOf[Method.Explicit]
      val parApp = defn.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Application.Prefix]
      val diag = parApp.diagnostics.collect {
        case d: IR.Warning.FailedParallelism => d
      }
      diag.length shouldEqual 1
      val expected = "Auto-parallel calls cannot depend on input arguments " +
        "as these are not statically known."
      diag.head.reason shouldEqual expected
    }

    "raise a warning when an intermediary is used outside the streams" in {
      implicit val moduleContext: ModuleContext = mkModuleContext

      val ir =
        """
          |fn f g =
          |    x = File.read "x"
          |    y = File.read "y"
          |    a = x.length
          |    b = y.length
          |    z = @Auto_Parallel Table.foo a b
          |    z y
          |""".stripMargin.preprocessModule.analyse

      val defn = ir.bindings.head.asInstanceOf[Method.Explicit]
      val parApp = defn.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .expressions(4)
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Application.Prefix]
      val diag = parApp.diagnostics.collect {
        case d: IR.Warning.FailedParallelism => d
      }
      diag.length shouldEqual 1
      val expected = "Cannot inline the dependencies of the incoming flows. " +
        "They are used in more than one place."
      diag.head.reason shouldEqual expected
    }

    "raise a warning when dependencies are used outside the @Auto_Parallel call" in {
      implicit val moduleContext: ModuleContext = mkModuleContext

      val ir =
        """
          |fn f g =
          |    x = File.read "x"
          |    y = File.read "y"
          |    a = x.length
          |    b = y.length
          |    f b
          |    z = @Auto_Parallel Table.foo a b
          |    f g
          |""".stripMargin.preprocessModule.analyse

      val defn = ir.bindings.head.asInstanceOf[Method.Explicit]
      val parApp = defn.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .expressions(5)
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Application.Prefix]
      val diag = parApp.diagnostics.collect {
        case d: IR.Warning.FailedParallelism => d
      }
      diag.length shouldEqual 1
      val expected = "Cannot inline the dependencies of the incoming flows. " +
        "They are used in more than one place."
      diag.head.reason shouldEqual expected
    }
  }
}
