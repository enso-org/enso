package org.enso.interpreter.test.instrument

import org.enso.interpreter.test.Metadata
import org.enso.common.LanguageInfo
import org.enso.common.RuntimeOptions
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.{
  InvalidatedExpressions,
  MethodCall,
  MethodPointer
}
import org.enso.text.{ContentVersion, Sha3_224VersionCalculator}
import org.enso.text.editing.model
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.logging.Level

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeAsyncCommandsTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  // === Test Utilities =======================================================

  var context: TestContext = _

  object Visualization {

    val metadata = new Metadata

    val code =
      metadata.appendToCode(
        """
          |encode x = (x + 1).to_text
          |""".stripMargin.linesIterator.mkString("\n")
      )

  }

  class TestContext(packageName: String)
      extends InstrumentTestContext(packageName) {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
    val context =
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName)
        .option(
          RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION,
          "false"
        )
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_EXECUTION_TIMER, "false")
        .option(
          RuntimeOptions.DISABLE_IR_CACHES,
          InstrumentTestContext.DISABLE_IR_CACHE
        )
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .option(RuntimeOptions.INTERACTIVE_MODE, "true")
        .option(
          RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
          Paths
            .get("../../test/micro-distribution/component")
            .toFile
            .getAbsolutePath
        )
        .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
        .out(out)
        .logHandler(System.err)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()

    def writeMain(contents: String): File =
      Files.write(pkg.mainFile.toPath, contents.getBytes).toFile

    def writeFile(file: File, contents: String): File =
      Files.write(file.toPath, contents.getBytes).toFile

    def writeInSrcDir(moduleName: String, contents: String): File = {
      val file = new File(pkg.sourceDir, s"$moduleName.enso")
      Files.write(file.toPath, contents.getBytes).toFile
    }

    def send(msg: Api.Request): Unit = runtimeServerEmulator.sendToRuntime(msg)

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

    def executionComplete(contextId: UUID): Api.Response =
      Api.Response(Api.ExecutionComplete(contextId))
  }

  def contentsVersion(content: String): ContentVersion =
    Sha3_224VersionCalculator.evalVersion(content)

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    context.init()
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }
  override protected def afterEach(): Unit = {
    if (context != null) {
      context.close()
      context.out.reset()
      context = null
    }
  }

  it should "interrupt stopped execution context" in {
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    val metadata = new Metadata
    val code =
      """main = "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    // interrupt
    context.send(
      Api.Request(requestId, Api.InterruptContextRequest(contextId))
    )
    context.receiveNIgnoreExpressionUpdates(
      1
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.InterruptContextResponse(contextId))
    )
  }

  it should "interrupt running execution context" in {
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    val metadata = new Metadata
    val code =
      """from Standard.Base import all
        |polyglot java import java.lang.Thread
        |
        |loop n s=0 =
        |    if (s > n) then s else
        |        Thread.sleep 100
        |        loop n s+1
        |
        |main =
        |    IO.println "started"
        |    loop 100
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreExpressionUpdates(
      1
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId))
    )

    // wait for program to start
    var isProgramStarted = false
    var iteration        = 0
    while (!isProgramStarted && iteration < 100) {
      val out = context.consumeOut
      Thread.sleep(200)
      isProgramStarted = out == List("started")
      iteration += 1
    }
    if (!isProgramStarted) {
      fail("Program start timed out")
    }

    // interrupt
    context.send(
      Api.Request(requestId, Api.InterruptContextRequest(contextId))
    )
    val responses = context.receiveNIgnoreExpressionUpdates(
      2
    )
    responses.length shouldEqual 2
    responses should contain(
      Api.Response(requestId, Api.InterruptContextResponse(contextId))
    )

    val failures = responses.filter(_.payload.isInstanceOf[Api.ExecutionFailed])
    failures.length shouldEqual 1

    val failure = failures.head.payload.asInstanceOf[Api.ExecutionFailed]
    failure.contextId shouldEqual contextId
    failure.result shouldBe a[Api.ExecutionResult.Diagnostic]

    val diagnostic = failure.result.asInstanceOf[Api.ExecutionResult.Diagnostic]
    diagnostic.kind shouldEqual Api.DiagnosticType.Error
    diagnostic.message shouldEqual Some("sleep interrupted")
    diagnostic.stack should not be empty
  }

  it should "recompute expression in context after interruption" in {
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    val metadata = new Metadata
    val idOut    = metadata.addItem(179, 17, "aa")

    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Input, Output
        |polyglot java import java.lang.Thread
        |
        |main =
        |    IO.println "started"
        |    loop 50
        |    out = Output.is_enabled
        |    IO.println out
        |
        |loop n=0 s=0 =
        |    if (s > n) then s else
        |        Thread.sleep 100
        |        loop n s+1
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.BOOLEAN,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("started", "False")

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idOut, Some(Api.ExecutionEnvironment.Live()))
          )
        )
      )
    )

    val responses = context.receiveNIgnorePendingExpressionUpdates(1)
    responses should contain(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId))
    )

    // wait for program to start and interrupt
    var isProgramStarted = false
    var iteration        = 0
    while (!isProgramStarted && iteration < 100) {
      val out = context.consumeOut
      Thread.sleep(200)
      isProgramStarted = out == List("started")
      iteration += 1
    }
    if (!isProgramStarted) {
      fail("Program start timed out")
    }
    context.consumeOut shouldEqual List()

    // trigger re-computation
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            model.TextEdit(
              model.Range(model.Position(10, 7), model.Position(10, 8)),
              "0"
            )
          ),
          execute = true,
          idMap   = None
        )
      )
    )
    val responses1 = context.receiveNIgnorePendingExpressionUpdates(3)
    responses1 should contain allOf (
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.BOOLEAN,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut should contain("True")
  }

  it should "interrupt running execution context without sending Panic in expression updates" in {
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    val metadata = new Metadata
    val vId      = metadata.addItem(194, 7)
    val code =
      """from Standard.Base import all
        |polyglot java import java.lang.Thread
        |
        |loop n s=0 =
        |    if (s > n) then s else
        |        Thread.sleep 100
        |        loop n s+1
        |
        |main =
        |    IO.println "started"
        |    v = loop 50
        |    v
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreExpressionUpdates(
      1
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId))
    )

    // wait for program to start
    var isProgramStarted = false
    var iteration        = 0
    while (!isProgramStarted && iteration < 100) {
      val out = context.consumeOut
      Thread.sleep(100)
      isProgramStarted = out == List("started")
      iteration += 1
    }
    if (!isProgramStarted) {
      fail("Program start timed out")
    }

    // recompute/interrupt
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(contextId, None, None, Seq())
      )
    )
    val responses = context.receiveNIgnoreStdLib(
      3
    )

    responses should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        vId,
        ConstantsGen.INTEGER,
        methodCall = Some(
          MethodCall(
            MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "loop"),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "interrupt running execution context without sending Panic in visualization updates" in {
    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualizationId = UUID.randomUUID()
    val moduleName      = "Enso_Test.Test.Main"
    val metadata        = new Metadata("import Standard.Base.Data.Numbers\n\n")

    val visualizationFile =
      context.writeInSrcDir("Visualization", Visualization.code)

    val idOp1 = metadata.addItem(203, 7)
    val idOp2 = metadata.addItem(227, 13)

    val code =
      """from Standard.Base import all
        |
        |polyglot java import java.lang.Thread
        |
        |loop n s=0 =
        |    if (s > n) then s else
        |        Thread.sleep 200
        |        loop n s+1
        |
        |main =
        |    IO.println "started"
        |    operator1 = loop 50
        |    operator2 = operator1 + 1
        |    operator2
        |
        |fun1 x = x.to_text
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open visualizations
    context.send(
      Api.Request(
        requestId,
        Api.OpenFileRequest(
          visualizationFile,
          Visualization.code
        )
      )
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // Open the new file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )

    // attach visualizations to both expressions
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualization(
          visualizationId,
          idOp2,
          Api.VisualizationConfiguration(
            contextId,
            Api.VisualizationExpression.Text(
              "Enso_Test.Test.Visualization",
              "x -> encode x",
              Vector()
            ),
            "Enso_Test.Test.Visualization"
          )
        )
      )
    )
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualization(
          visualizationId,
          idOp1,
          Api.VisualizationConfiguration(
            contextId,
            Api.VisualizationExpression.Text(
              "Enso_Test.Test.Visualization",
              "x -> encode x",
              Vector()
            ),
            "Enso_Test.Test.Visualization"
          )
        )
      )
    )

    val response1 = context.receiveNIgnoreExpressionUpdates(
      6,
      timeoutSeconds = 20
    )
    response1 should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(requestId, Api.VisualizationAttached()),
      context.executionComplete(contextId)
    )
    context.consumeOut
    response1
      .map(_.payload)
      .count(_.isInstanceOf[Api.VisualizationAttached]) should be(2)
    response1
      .map(_.payload)
      .count(_.isInstanceOf[Api.VisualizationUpdate]) should be(2)

    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(InvalidatedExpressions.Expressions(Vector(idOp1, idOp2))),
          None,
          Seq()
        )
      )
    )
    var isProgramStarted = false
    var iteration        = 0
    while (!isProgramStarted && iteration < 100) {
      val out = context.consumeOut
      Thread.sleep(100)
      isProgramStarted = out == List("started")
      iteration += 1
    }
    if (!isProgramStarted) {
      fail("Program start timed out")
    }

    // Trigger interruption
    context.send(
      Api.Request(requestId, Api.InterruptContextRequest(contextId))
    )
    val response2 = context.receiveNIgnoreExpressionUpdates(
      5,
      timeoutSeconds = 20
    )
    response2 should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      Api.Response(requestId, Api.InterruptContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    val failure = response2.collectFirst({
      case Api.Response(None, Api.VisualizationEvaluationFailed(_, msg, _)) =>
        msg
    })
    failure should be(Symbol("empty"))
  }

}
