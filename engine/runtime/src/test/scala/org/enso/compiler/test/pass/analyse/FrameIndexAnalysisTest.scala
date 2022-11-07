package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.analyse.FrameIndexAnalysis
import org.enso.compiler.pass.analyse.FrameIndexAnalysis.FrameInfo.FrameIndex
import org.enso.compiler.pass.analyse.FrameIndexAnalysis.FrameInfo.LocalVariables
import org.enso.compiler.test.CompilerTest

import scala.List
import scala.collection.mutable.ListBuffer


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

  private def getNamesForVariables(module: IR, localVars: LocalVariables): List[String] = {
    val localVarsNames: ListBuffer[String] = new ListBuffer()
    localVars.variables.foreach(localVarId => {
      val localVarIr = module.preorder.find(ir => ir.getId == localVarId)
      localVarIr.get match {
        case binding: IR.Expression.Binding =>
          localVarsNames += binding.name.name
        case defArg: IR.DefinitionArgument =>
          localVarsNames += defArg.name.name
        case _ => fail(s"Unexpected IR: ${localVarIr.get}")
      }
    })
    localVarsNames.toList
  }

  "Pass configuration" should {
    // TODO: Test FrameIndexAnalysis pass configuration
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
          val localVars = metadata.get.as[LocalVariables]
          localVars shouldBe defined
          localVars.get
        })
        .head
      localVariables.variables should not be empty

      val argFrameIndex = module.preorder
        .collect { case x: IR.DefinitionArgument => x }
        .map(argument => {
          val argMetadata = argument.getMetadata(FrameIndexAnalysis)
          argMetadata shouldBe defined
          val frameIndex = argMetadata.get.as[FrameIndex]
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
          val frameIndex = metadata.get.as[FrameIndex]
          frameIndex shouldBe defined
          frameIndex.get
        })
        .head
      // There is synthetic self as an implicit argument
      frameIndex.index shouldBe 1
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

      val locVarNames = getNamesForVariables(module, myFuncMethodLocVars)
      locVarNames should equal (List("self", "x", "y"))

      xArgFrameIdx.index shouldBe 1
      yBindingFrameIdx.index shouldBe 2
      myFuncMethodLocVars.variables.length shouldBe 3
    }

    "Nested function with arguments" in {
      val module =
        """
          |my_func =
          |    x = 10
          |    nested_func y = x + y
          |    nested_func x
          |""".stripMargin.preprocessModule.analyseFrameIndexes
      val myFunc = findMethod(module, "my_func")
        .body
        .asInstanceOf[IR.Function]
      val nestedFunc = findBinding(module, "nested_func")
        .expression
        .asInstanceOf[IR.Function]
      val myFuncLocVars = getLocalVariablesMetadata(myFunc)
      val nestedFuncLocVars = getLocalVariablesMetadata(nestedFunc)

      getNamesForVariables(module, myFuncLocVars) should equal (List("self", "x", "nested_func"))
      getNamesForVariables(module, nestedFuncLocVars) should equal (List("y"))
    }

    "Nested function with arguments and local variables" in {
      val module =
        """
          |my_func =
          |    nested_func x =
          |        y = x + 1
          |        y
          |    nested_func 42
          |""".stripMargin.preprocessModule.analyseFrameIndexes
      val myFunc = findMethod(module, "my_func")
        .body
        .asInstanceOf[IR.Function]
      val nestedFunc = findBinding(module, "nested_func")
        .expression
        .asInstanceOf[IR.Function]
      val myFuncLocVars = getNamesForVariables(
        module,
        getLocalVariablesMetadata(myFunc)
      )
      val nestedFuncLocVars = getNamesForVariables(
        module,
        getLocalVariablesMetadata(nestedFunc)
      )

      myFuncLocVars should equal (List("self", "nested_func"))
      nestedFuncLocVars should equal (List("x", "y"))
    }

  }

  "Anonymous functions" should {
    implicit val moduleContext: ModuleContext =
      buildModuleContext(
        passConfiguration = Some(passConfig),
        freshNameSupply = Some(new FreshNameSupply())
      )

    val module =
      """
        |apply : Integer -> (Integer -> Integer) -> Integer
        |apply x func = func(x)
        |
        |my_func =
        |    apply 42 x->
        |        y = x + 1
        |        y
        |""".stripMargin.preprocessModule.analyseFrameIndexes

    // Anonymous function passed as an argument to apply method
    val anonFunc = module
      .bindings(1)
      .asInstanceOf[IR.Module.Scope.Definition.Method]
      .body
      .preorder
      .collect {case x: IR.Function => x}
      .last

    "Contain local vars metadata" in {
      val anonFuncVarNames = getNamesForVariables(
        module,
        getLocalVariablesMetadata(anonFunc)
      )
      anonFuncVarNames should equal (List("x", "y"))
    }

    "Argument has FrameIndex metadata" in {
      val xArg = anonFunc
        .children
        .collect {case arg: IR.DefinitionArgument => arg}
        .head
      val frameIdx = getFrameIndexMetadata(xArg)
      frameIdx.index shouldBe 0
    }

    "Local variable has FrameIndex metadata" in {
      val yLocVar = anonFunc
        .preorder
        .collect {case binding: IR.Expression.Binding => binding}
        .last
      val frameIdx = getFrameIndexMetadata(yLocVar)
      frameIdx.index shouldBe 1
    }
  }
}
