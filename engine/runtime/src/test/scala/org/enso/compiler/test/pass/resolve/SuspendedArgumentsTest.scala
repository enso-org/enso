package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.resolve.SuspendedArguments
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.compiler.pass.PassConfiguration._

class SuspendedArgumentsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(SuspendedArguments).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to a module for performing suspended argument
    * resolution.
    *
    * @param ir the IR to add the extension method to
    */
  implicit class ResolveModule(ir: IR.Module) {

    /** Resolves suspended arguments in [[ir]].
      *
      * @param moduleContext the context in which resolution is taking place
      * @return [[ir]], with all suspended arguments resolved
      */
    def resolve(implicit moduleContext: ModuleContext): IR.Module = {
      SuspendedArguments.runModule(ir, moduleContext)
    }
  }

  /** Adds an extension method to an expression for performing suspended
    * argument resolution.
    *
    * @param ir the expression to add the extension method to
    */
  implicit class ResolveExpression(ir: IR.Expression) {

    /** Resolves suspended arguments in [[ir]].
      *
      * @param inlineContext the context in which resolution is taking place
      * @return [[ir]], with all suspended arguments resolved
      */
    def resolve(implicit inlineContext: InlineContext): IR.Expression = {
      SuspendedArguments.runExpression(ir, inlineContext)
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
    buildInlineContext(
      freshNameSupply = Some(new FreshNameSupply),
      localScope      = Some(LocalScope.root)
    )
  }

  // === The Tests ============================================================

  "Suspended arguments resolution in modules" should {
    "correctly mark arguments as suspended based on their type signatures" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |Any.id : Suspended -> a
          |Any.id self a = a
          |""".stripMargin.preprocessModule.resolve.bindings.head
          .asInstanceOf[Method]

      val bodyLam = ir.body.asInstanceOf[IR.Function.Lambda]

      bodyLam.arguments.length shouldEqual 2
      assert(
        !bodyLam.arguments.head.suspended,
        "the `self` argument is suspended"
      )
      assert(
        bodyLam.arguments(1).suspended,
        "the `a` argument is not suspended"
      )
    }

    "work recursively" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |Any.id : Suspended -> a
          |Any.id a =
          |    lazy_id : Suspended -> a
          |    lazy_id x = x
          |
          |    lazy_id a
          |""".stripMargin.preprocessModule.resolve.bindings.head
          .asInstanceOf[Method]

      val bodyBlock = ir.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]

      val lazyId = bodyBlock.expressions.head
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Function.Lambda]

      assert(lazyId.arguments.head.suspended, "x was not suspended")
    }

    "work for more complex signatures" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |File.with_output_stream : Vector.Vector -> (Output_Stream -> Any ! File_Error) -> Any ! File_Error
          |File.with_output_stream open_options action = undefined
          |""".stripMargin.preprocessModule.resolve.bindings.head
          .asInstanceOf[Method.Explicit]

      val bodyLam = ir.body.asInstanceOf[IR.Function.Lambda]

      bodyLam.arguments.length shouldEqual 3

      assert(!bodyLam.arguments(1).suspended, "open_options was suspended")
      assert(!bodyLam.arguments(2).suspended, "action was suspended")
    }

    "work for conversion methods" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """File.from : Text -> Suspended -> Any
          |File.from (that : Text) config=Nothing = undefined
          |""".stripMargin.preprocessModule.resolve.bindings.head
          .asInstanceOf[Method.Conversion]

      val bodyLam = ir.body.asInstanceOf[IR.Function.Lambda]
      val args    = bodyLam.arguments

      args.length shouldEqual 3
      assert(!args(1).suspended, "the source argument was suspended")
      assert(args(2).suspended, "the config argument was not suspended")
    }

    "raise an error if a conversion method marks its source argument as suspended" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """File.from (~that : Text) = undefined
          |""".stripMargin.preprocessModule.resolve.bindings.head

      ir shouldBe an[IR.Error.Conversion]
      ir.asInstanceOf[IR.Error.Conversion]
        .reason shouldBe an[IR.Error.Conversion.SuspendedSourceArgument]
    }

    "work for local functions applications" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |foo : Number -> Number -> Any
          |foo self ~a = self+a
          |
          |foo 1 1
          |""".stripMargin.preprocessModule.resolve.bindings.head
          .asInstanceOf[Method]

      val lam = ir.body
        .asInstanceOf[IR.Function.Lambda]
      val bodyBlock = lam.body
        .asInstanceOf[IR.Application.Prefix]

      lam.arguments.length shouldEqual 2
      lam.arguments(1).suspended shouldBe true
      bodyBlock.arguments.length shouldEqual 2
    }

  }

  "Suspended arguments resolution in expressions" should {
    "correctly mark arguments as suspended in blocks" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |f : a -> Suspended -> b
          |f a b = b
          |""".stripMargin.preprocessExpression.get.resolve
          .asInstanceOf[IR.Expression.Block]

      val func = ir.returnValue
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Function.Lambda]
      assert(!func.arguments.head.suspended, "a is suspended")
      assert(func.arguments(1).suspended, "b is not suspended")
    }

    "correctly mark arguments as suspended using inline expressions" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(x -> y -> y + x) : Suspended -> a -> a
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[IR.Function.Lambda]
      val lam = ir.asInstanceOf[IR.Function.Lambda]
      assert(lam.arguments.head.suspended, "x is not suspended")
      assert(!lam.arguments(1).suspended, "y is suspended")
    }

    "work recursively" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x -> y ->
          |    f : a -> Suspended -> a
          |    f a b = a + b
          |
          |    f 100 50
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[IR.Function.Lambda]
      val lam = ir.asInstanceOf[IR.Function.Lambda]
      val f = lam.body
        .asInstanceOf[IR.Expression.Block]
        .expressions
        .head
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Function.Lambda]

      assert(!f.arguments.head.suspended, "a was suspended")
      assert(f.arguments(1).suspended, "b was not suspended")
    }
  }
}
