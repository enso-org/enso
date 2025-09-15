package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Function.Lambda
import org.enso.compiler.core.ir.{DefinitionArgument, Module, Name}
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.Type
import org.enso.compiler.pass.resolve.MethodDefinitions
import org.enso.compiler.pass.{
  MiniIRPass,
  PassConfiguration,
  PassGroup,
  PassManager
}
import org.enso.compiler.test.CompilerTest

class MethodDefinitionsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(MethodDefinitions.INSTANCE).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext): Module = {
      val miniPass =
        MethodDefinitions.INSTANCE.createForModuleCompilation(context)
      MiniIRPass.compile(classOf[Module], ir, miniPass)
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
        .asInstanceOf[definition.Method.Explicit]
        .methodReference
        .typePointer
        .get
        .getMetadata(
          MethodDefinitions.INSTANCE,
          classOf[BindingsMap.Resolution]
        ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Foo", List("a", "b", "c"), List(), false, false)
          )
        )
      )
      ir.bindings(3)
        .asInstanceOf[definition.Method.Explicit]
        .methodReference
        .typePointer shouldBe None

      ir.bindings(4)
        .asInstanceOf[definition.Method.Explicit]
        .methodReference
        .typePointer
        .get
        .getMetadata(
          MethodDefinitions.INSTANCE,
          classOf[BindingsMap.Resolution]
        ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedModule(ctx.moduleReference())
        )
      )

      ir.bindings(5)
        .asInstanceOf[definition.Method.Explicit]
        .methodReference
        .typePointer
        .get shouldBe a[errors.Resolution]

      val conv1 = ir
        .bindings(6)
        .asInstanceOf[definition.Method.Conversion]
      conv1.methodReference.typePointer.get.getMetadata(
        MethodDefinitions.INSTANCE,
        classOf[BindingsMap.Resolution]
      ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Foo", List("a", "b", "c"), List(), false, false)
          )
        )
      )
      conv1.sourceTypeName.getMetadata(
        MethodDefinitions.INSTANCE,
        classOf[BindingsMap.Resolution]
      ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Bar", List(), List(), false, false)
          )
        )
      )

      val conv2 = ir
        .bindings(7)
        .asInstanceOf[definition.Method.Conversion]
      conv2.methodReference.typePointer.get.getMetadata(
        MethodDefinitions.INSTANCE,
        classOf[BindingsMap.Resolution]
      ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Bar", List(), List(), false, false)
          )
        )
      )
      conv2.sourceTypeName shouldBe an[errors.Resolution]

      val conv3 = ir
        .bindings(8)
        .asInstanceOf[definition.Method.Conversion]
      conv3.methodReference.typePointer.get shouldBe an[errors.Resolution]
      conv3.sourceTypeName.getMetadata(
        MethodDefinitions.INSTANCE,
        classOf[BindingsMap.Resolution]
      ) shouldEqual Some(
        BindingsMap.Resolution(
          BindingsMap.ResolvedType(
            ctx.moduleReference(),
            Type("Foo", List("a", "b", "c"), List(), false, false)
          )
        )
      )
    }
  }

  "Method definitions pass" should {
    implicit val ctx: ModuleContext = mkModuleContext

    "Attach ascribedType to DefinitionArgument of static method" in {
      val ir =
        """
          |type My_Type
          |    Value x
          |    f self = self
          |""".stripMargin.preprocessModule.analyse
      val staticMethod = ir.bindings.find { binding =>
        binding match {
          case m: Method.Explicit if m.isStatic => true
          case _                                => false
        }
      }
      staticMethod.isDefined shouldBe true
      val method = staticMethod.get.asInstanceOf[Method.Explicit]
      method.isStatic shouldBe true
      val lambdaArg = method.body
        .asInstanceOf[Lambda]
        .body
        .asInstanceOf[Lambda]
        .arguments
        .head
        .asInstanceOf[DefinitionArgument.Specified]
      lambdaArg.ascribedType().isDefined shouldBe true
    }

    "Method.Explicit is duplicated with synthetic self arg for non-singleton types" in {
      val ir =
        """
          |type My_Type
          |    Value x
          |    f self = 42
          |""".stripMargin.preprocessModule.analyse
      val methods = ir.bindings.collect {
        case m: Method.Explicit if m.methodReference.methodName.name == "f" => m
      }
      methods.size shouldBe 2
      val method_1 = methods(0)
      val method_2 = methods(1)
      method_1.isStatic shouldBe false
      method_2.isStatic shouldBe true

      // Check args
      val method1_args = methodArgs(method_1)
      val method2_args = methodArgs(method_2)
      method1_args.size shouldBe 1
      method2_args.size shouldBe 1
      val method1_selfArg = method1_args.head.name().asInstanceOf[Name.Self]
      val method2_selfArg = method2_args.head.name().asInstanceOf[Name.Self]
      method1_selfArg.synthetic shouldBe false
      method2_selfArg.synthetic shouldBe true

      val method2_nestedLambda =
        method_2.body.asInstanceOf[Lambda].body().asInstanceOf[Lambda]
      val method2_nestedLambdaArgs = method2_nestedLambda.arguments()
      method2_nestedLambdaArgs.size shouldBe 1
      val method2_nestedLambdaArg = method2_nestedLambdaArgs.head
      method2_nestedLambdaArg.name().isInstanceOf[Name.Self] shouldBe true
      method2_nestedLambdaArg.ascribedType().isDefined shouldBe true
      method2_nestedLambdaArg
        .ascribedType()
        .get
        .isInstanceOf[Name.SelfType] shouldBe true
    }

    "No Method.Explicit duplication for singleton type" in {
      val ir =
        """
          |type My_Type
          |    f self = 42
          |""".stripMargin.preprocessModule.analyse
      val methods = ir.bindings.collect {
        case m: Method.Explicit if m.methodReference.methodName.name == "f" => m
      }
      methods.size shouldBe 1
    }
  }

  private def methodArgs(
    method: Method.Explicit
  ): List[DefinitionArgument] = {
    method.body.asInstanceOf[Lambda].arguments
  }
}
