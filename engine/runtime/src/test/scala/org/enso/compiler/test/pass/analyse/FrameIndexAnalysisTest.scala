package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.analyse.FrameIndexAnalysis
import org.enso.compiler.test.CompilerTest


class FrameIndexAnalysisTest extends CompilerTest {
  val passes = new Passes(defaultConfig)
  val precursorPasses: PassGroup = passes.getPrecursors(FrameIndexAnalysis).get
  val passConfig: PassConfiguration = PassConfiguration(
    FrameIndexAnalysis -->> FrameIndexAnalysis.Configuration(
      shouldWriteToContext = false,
      initialFrameIndexGap = 0
    )
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  implicit class AnalyseModule(ir: IR.Module) {
    def analyseFrameIndexes: IR.Module = {
      FrameIndexAnalysis.runModule(
        ir,
        buildModuleContext(
          passConfiguration = Some(passConfig)
        )
      )
    }
  }

  implicit class AnalyseExpression(expr: IR.Expression) {
    def analyseFrameIndexes(inlineContext: InlineContext): IR.Expression = {
      FrameIndexAnalysis.runExpression(expr, inlineContext)
    }
  }

  private def findMethod(ir: IR, methodName: String): IR.Module.Scope.Definition.Method = {
    val methods = ir
      .preorder
      .collect { case x: IR.Module.Scope.Definition.Method => x }
      .filter(method =>
        method.methodName.name == methodName
      )
    assume(methods.length == 1)
    methods.head
  }

  private def findBinding(ir: IR, bindingName: String): IR.Expression.Binding = {
    val bindings = ir
      .preorder
      .collect { case x: IR.Expression.Binding => x}
      .filter(binding => {
        binding.name.name == bindingName
      })
    assume(bindings.length == 1)
    bindings.head
  }

  private def findArgument(ir: IR, argumentName: String): IR.DefinitionArgument = {
    val arguments = ir
      .preorder
      .collect { case x: IR.DefinitionArgument => x}
      .filter(argument => {
        argument.name.name == argumentName
      })
    assume(arguments.length == 1)
    arguments.head
  }

  private def getFrameIndexMetadata(ir: IR): FrameIndexAnalysis.FrameInfo.FrameIndex = {
    val metadata = ir.getMetadata(FrameIndexAnalysis)
    metadata shouldBe defined
    val frameIndex = metadata.get.as[FrameIndexAnalysis.FrameInfo.FrameIndex]
    frameIndex shouldBe defined
    frameIndex.get
  }

  private def getLocalVariablesMetadata(ir: IR): FrameIndexAnalysis.FrameInfo.LocalVariables = {
    val metadata = ir.getMetadata(FrameIndexAnalysis)
    metadata shouldBe defined
    val localVars = metadata.get.as[FrameIndexAnalysis.FrameInfo.LocalVariables]
    localVars shouldBe defined
    localVars.get
  }

  "Frame index analysis for simple scopes" should {
    implicit val moduleContext: ModuleContext =
      buildModuleContext(
        passConfiguration = Some(passConfig),
        freshNameSupply = Some(new FreshNameSupply())
      )

    "Metadata are attached to functions and arguments" in {
      val module = """
        |my_func x = x
        |""".stripMargin.preprocessModule.analyseFrameIndexes

      val localVariables = module.preorder
        .collect { case x: IR.Function => x }
        .map(function => {
          val metadata = function.getMetadata(FrameIndexAnalysis)
          metadata shouldBe defined
          val localVars = metadata.get.as[FrameIndexAnalysis.FrameInfo.LocalVariables]
          localVars shouldBe defined
          localVars.get
        })
        .head
      localVariables.variables shouldBe empty

      val argFrameIndex = module.preorder
        .collect { case x: IR.DefinitionArgument => x }
        .map(argument => {
          val argMetadata = argument.getMetadata(FrameIndexAnalysis)
          argMetadata shouldBe defined
          val frameIndex = argMetadata.get.as[FrameIndexAnalysis.FrameInfo.FrameIndex]
          frameIndex shouldBe defined
          frameIndex.get
        })
        .head
      argFrameIndex.index shouldBe 0
    }

    "Metadata are attached to local variables" in {
      val module = """
          |my_func =
          |    x = 10
          |    x
          |""".stripMargin.preprocessModule.analyseFrameIndexes

      val frameIndex = module
        .preorder
        .collect { case x: IR.Expression.Binding => x}
        .map(binding => {
          val metadata = binding.getMetadata(FrameIndexAnalysis)
          metadata shouldBe defined
          val frameIndex = metadata.get.as[FrameIndexAnalysis.FrameInfo.FrameIndex]
          frameIndex shouldBe defined
          frameIndex.get
        })
        .head
      frameIndex.index shouldBe 0
    }

    "Argument and local var are in method's scope" in {
      val module =
        """
          |my_func x =
          |    y = x + 10
          |    y
          |""".stripMargin.preprocessModule.analyseFrameIndexes

      val xArgument = findArgument(module, "x")
      val yBinding = findBinding(module, "y")
      val myFuncMethod = findMethod(module, "my_func")

      val xArgFrameIdx = getFrameIndexMetadata(xArgument)
      val yBindingFrameIdx = getFrameIndexMetadata(yBinding)
      val myFuncMethodLocVars = getLocalVariablesMetadata(myFuncMethod.body)

      xArgFrameIdx.index shouldBe 0
      yBindingFrameIdx.index shouldBe 1
      myFuncMethodLocVars.variables.length shouldBe 2
    }
  }
}
