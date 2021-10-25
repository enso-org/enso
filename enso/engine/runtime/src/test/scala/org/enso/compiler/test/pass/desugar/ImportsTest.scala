package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.desugar.Imports
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ImportsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(Imports).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: IR.Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext) = {
      Imports.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Main import and export desugaring" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |import Bar.Foo
        |import Bar.Foo as Bar
        |from Bar.Foo import Bar, Baz
        |from Bar.Foo as Bar import Baz, Spam
        |
        |export Bar.Foo
        |export Bar.Foo as Bar
        |from Bar.Foo export Bar, Baz
        |from Bar.Foo as Bar export all
        |
        |import Foo.Bar.Baz
        |export Foo.Bar.Baz
        |""".stripMargin.preprocessModule.analyse

    "desugar project name imports correctly" in {
      ir.imports.take(4).map(_.showCode()) shouldEqual List(
        "import Bar.Foo.Main as Foo",
        "import Bar.Foo.Main as Bar",
        "from Bar.Foo.Main as Foo import Bar, Baz",
        "from Bar.Foo.Main as Bar import Baz, Spam"
      )
    }

    "desugar project name exports correctly" in {
      ir.exports.take(4).map(_.showCode()) shouldEqual List(
        "export Bar.Foo.Main as Foo",
        "export Bar.Foo.Main as Bar",
        "from Bar.Foo.Main as Foo export Bar, Baz",
        "from Bar.Foo.Main as Bar export all"
      )
    }

    "leave module imports and exports untouched" in {
      ir.imports.last.showCode() shouldEqual "import Foo.Bar.Baz"
      ir.exports.last.showCode() shouldEqual "export Foo.Bar.Baz"
    }

  }
}
