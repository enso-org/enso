package org.enso.interpreter.test.instrument

import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.logging.Level

@scala.annotation.nowarn("msg=multiarg infix syntax")
class BuiltinTypesTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  // === Test Utilities =======================================================

  var context: TestContext = _

  class TestContext(packageName: String)
      extends InstrumentTestContext(packageName) {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
    val context =
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
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
        .logHandler(System.err)
        .out(out)
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

  def runCode(contextId: UUID, requestId: UUID, contents: String): Unit = {
    val moduleName = "Enso_Test.Test.Main"

    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
  }

  it should "send updates of an Integer type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 7)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    42
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Float type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 8)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    4.2
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.FLOAT_BUILTIN),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Boolean type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 9)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    True
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.BOOLEAN),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Text type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 9)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    "42"
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.TEXT),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Function type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idY      = metadata.addItem(46, 6)
    val idMain   = metadata.addItem(37, 24)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    y = x -> x
        |    y 42
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idY, ConstantsGen.FUNCTION),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of an Atom type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(65, 15)

    val code =
      """from Standard.Base import all
        |
        |type Foo
        |    Bar a
        |    Baz
        |
        |main =
        |    Foo.Bar 42
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, "Enso_Test.Test.Main.Foo"),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of an Atom constructor type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(65, 12)

    val code =
      """from Standard.Base import all
        |
        |type Foo
        |    Bar a
        |    Baz
        |
        |main =
        |    Foo.Bar
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMain,
        ConstantsGen.FUNCTION,
        payload = Api.ExpressionUpdate.Payload.Value(
          functionSchema = Some(
            Api.FunctionSchema(
              Api.MethodPointer(
                "Enso_Test.Test.Main",
                "Enso_Test.Test.Main.Foo",
                "Bar"
              ),
              Vector(0)
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of an Array type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(40, 18)

    val code =
      """from Standard.Base import Vector
        |
        |main =
        |    [42].to_array
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.ARRAY),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Vector type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(40, 7)

    val code =
      """from Standard.Base import Vector
        |
        |main =
        |    []
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.VECTOR),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Ref type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(40, 15)

    val code =
      """import Standard.Base.Runtime.Ref
        |
        |main =
        |    Ref.new 42
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.REF),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Date type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(48, 25)

    val code =
      """import Standard.Base.Data.Time.Date
        |
        |main =
        |    Date.new_builtin 2000 1 1
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.DATE),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Date_Time type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(48, 18)

    val code =
      """import Standard.Base.Data.Time.Date_Time
        |
        |main =
        |    Date_Time.now
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.DATE_TIME),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Time_Of_Day type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(50, 20)

    val code =
      """import Standard.Base.Data.Time.Time_Of_Day
        |
        |main =
        |    Time_Of_Day.now
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.TIME_OF_DAY),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Time_Zone type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(48, 18)

    val code =
      """import Standard.Base.Data.Time.Time_Zone
        |
        |main =
        |    Time_Zone.new
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.TIME_ZONE),
      context.executionComplete(contextId)
    )
  }

  it should "send updates of a Nothing type" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 30)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    runCode(contextId, requestId, contents)

    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
  }

}
