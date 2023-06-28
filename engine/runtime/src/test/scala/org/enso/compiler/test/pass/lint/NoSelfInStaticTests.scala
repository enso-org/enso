package org.enso.compiler.test.pass.lint

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.lint.NoSelfInStatic
import org.enso.compiler.test.CompilerTest

class NoSelfInStaticTests extends CompilerTest {
  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(NoSelfInStatic).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method for running linting on the input IR.
    *
    * @param ir the IR to lint
    */
  implicit class LintModule(ir: IR.Module) {

    /** Runs unused name linting on [[ir]].
      *
      * @param moduleContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all unused names linted
      */
    def lint(implicit moduleContext: ModuleContext): IR.Module = {
      NoSelfInStatic.runModule(ir, moduleContext)
    }
  }

  /** Makes a module context.
    *
    * @return a new inline context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "No self argument in static methods linting" should {
    "generate an error when self argument is used in a type static method" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type Foo
          |    Value x
          |    bar = self.x + self.x
          |""".stripMargin.preprocessModule.lint
      val errs = ir.bindings.flatMap(_.preorder).collect {
        case err @ IR.Error
              .Syntax(_, IR.Error.Syntax.InvalidSelfArgUsage, _, _) =>
          err
      }
      errs should have size 2
    }

    "generate an error when self argument is used in a module static method" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |static_method x y = x + y + self.data
          |""".stripMargin.preprocessModule.lint
      val errs = ir.bindings.flatMap(_.preorder).collect {
        case err @ IR.Error
              .Syntax(_, IR.Error.Syntax.InvalidSelfArgUsage, _, _) =>
          err
      }
      errs should have size 1
    }

    "generate an error when self argument is used in a nested static method" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |static_method x y =
          |    nested_method z =
          |        tmp = z + x + y
          |        self.data + tmp
          |    nested_method (x + y)
          |""".stripMargin.preprocessModule.lint
      val errs = ir.bindings.flatMap(_.preorder).collect {
        case err @ IR.Error
              .Syntax(_, IR.Error.Syntax.InvalidSelfArgUsage, _, _) =>
          err
      }
      errs should have size 1
    }

    "succeed when self argument is used in a nested instance method" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    Value value
          |
          |    instance_method self =
          |        nested_method x = self.value + 1
          |        nested_method 42
          |""".stripMargin.preprocessModule.lint
      val errs = ir.bindings.flatMap(_.preorder).collect {
        case err @ IR.Error
              .Syntax(_, IR.Error.Syntax.InvalidSelfArgUsage, _, _) =>
          err
      }
      errs should be(empty)
    }

    "generate an error when self argument is used in a static extension method" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    Value value
          |
          |My_Type.extension_method = self.value + 1
          |""".stripMargin.preprocessModule.lint
      val errs = ir.bindings.flatMap(_.preorder).collect {
        case err @ IR.Error
              .Syntax(_, IR.Error.Syntax.InvalidSelfArgUsage, _, _) =>
          err
      }
      errs should have size 1
    }
  }
}
