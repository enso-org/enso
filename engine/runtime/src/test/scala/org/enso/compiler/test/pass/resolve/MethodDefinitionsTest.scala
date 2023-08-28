package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.Type
import org.enso.compiler.pass.resolve.MethodDefinitions
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class MethodDefinitionsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(MethodDefinitions).get

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
    def analyse(implicit context: ModuleContext): IR.Module = {
      MethodDefinitions.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Method definition resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |type Foo a b c
        |type Bar
        |
        |Foo.my_method a b c = a + b + c
        |
        |my_method = 10
        |
        |Test_Module.other_method = 11
        |
        |Does_Not_Exist.method = 32
        |
        |Foo.from (that : Bar) = undefined
        |
        |Bar.from (that : Does_Not_Exist) = undefined
        |
        |Does_Not_Exist.from (that : Foo) = undefined
        |""".stripMargin.preprocessModule.analyse

    "attach resolved atoms to the method definitions" in {
      ir.bindings(2)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .methodReference
        .typePointer
        .get
        .getMetadata(MethodDefinitions) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Foo", List(), false)
          )
        )
      )
      ir.bindings(3)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .methodReference
        .typePointer shouldBe None

      ir.bindings(4)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .methodReference
        .typePointer
        .get
        .getMetadata(MethodDefinitions) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedModule(ctx.moduleReference())
        )
      )

      ir.bindings(5)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .methodReference
        .typePointer
        .get shouldBe a[IR.Error.Resolution]

      val conv1 = ir
        .bindings(6)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Conversion]
      conv1.methodReference.typePointer.get.getMetadata(
        MethodDefinitions
      ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Foo", List(), false)
          )
        )
      )
      conv1.sourceTypeName.getMetadata(MethodDefinitions) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Bar", List(), false)
          )
        )
      )

      val conv2 = ir
        .bindings(7)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Conversion]
      conv2.methodReference.typePointer.get.getMetadata(
        MethodDefinitions
      ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Bar", List(), false)
          )
        )
      )
      conv2.sourceTypeName shouldBe an[IR.Error.Resolution]

      val conv3 = ir
        .bindings(8)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Conversion]
      conv3.methodReference.typePointer.get shouldBe an[IR.Error.Resolution]
      conv3.sourceTypeName.getMetadata(MethodDefinitions) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Foo", List(), false)
          )
        )
      )
    }
  }
}
