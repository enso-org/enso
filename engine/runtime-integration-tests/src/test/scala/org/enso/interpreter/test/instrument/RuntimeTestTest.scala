package org.enso.interpreter.test.instrument

import org.apache.commons.io.output.TeeOutputStream
import org.enso.common.{LanguageInfo, MethodNames, RuntimeOptions}
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Paths}
import java.util.UUID

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeTestTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String)
      extends InstrumentTestContext(packageName)
      with RuntimeServerTest.TestMain {

    val out: ByteArrayOutputStream    = new ByteArrayOutputStream()
    val logOut: ByteArrayOutputStream = new ByteArrayOutputStream()
    protected val context =
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(
          RuntimeOptions.LOG_LEVEL,
          java.util.logging.Level.INFO.getName
        )
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_EXECUTION_TIMER, "false")
        .option(RuntimeOptions.STRICT_ERRORS, "false")
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
        .logHandler(new TeeOutputStream(logOut, System.err))
        .out(new TeeOutputStream(out, System.err))
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()

    lazy val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[EnsoContext]

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

  it should "edit local call with cached self" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata   = new Metadata
    val idX        = metadata.addItem(46, 11, "aa")
    val idSelfMain = metadata.addItem(46, 4, "ab")
    val idIncZ     = new UUID(0, 1)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = Main.inc 41
        |    IO.println x
        |
        |inc a =
        |    y = 1
        |    r = a + y
        |    r
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idX,
        ConstantsGen.INTEGER,
        methodCall =
          Some(Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "inc")))
      ),
      TestMessages.update(contextId, idSelfMain, moduleName),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // push inc call
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(idX))
      )
    )
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            model.TextEdit(
              model.Range(model.Position(8, 4), model.Position(8, 9)),
              "z = 2\n    r = a + z"
            )
          ),
          execute = true,
          idMap   = Some(model.IdMap(Vector(model.Span(102, 103) -> idIncZ)))
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(2, 10) shouldEqual Seq(
      TestMessages.update(
        contextId,
        idIncZ,
        ConstantsGen.INTEGER
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("44")
  }

  it should "edit two local calls with cached self" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idX         = metadata.addItem(46, 10, "aa")
    val idY         = metadata.addItem(65, 10, "ab")
    val idXSelfMain = metadata.addItem(46, 4, "ac")
    val idYSelfMain = metadata.addItem(65, 4, "ad")
    val idIncZ      = new UUID(0, 1)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = Main.inc 3
        |    y = Main.inc 7
        |    IO.println x+y
        |
        |inc a =
        |    y = 1
        |    r = a + y
        |    r
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idX,
        ConstantsGen.INTEGER,
        methodCall =
          Some(Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "inc")))
      ),
      TestMessages.update(
        contextId,
        idY,
        ConstantsGen.INTEGER,
        methodCall =
          Some(Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "inc")))
      ),
      TestMessages.update(contextId, idXSelfMain, moduleName),
      TestMessages.update(contextId, idYSelfMain, moduleName),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("12")

    // push inc call
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(idX))
      )
    )
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("12")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            model.TextEdit(
              model.Range(model.Position(9, 4), model.Position(9, 9)),
              "z = 2\n    r = a + z"
            )
          ),
          execute = true,
          idMap   = Some(model.IdMap(Vector(model.Span(122, 123) -> idIncZ)))
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(2, 10) shouldEqual Seq(
      TestMessages.update(
        contextId,
        idIncZ,
        ConstantsGen.INTEGER
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("16")

    // pop the inc call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))

    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idX,
        ConstantsGen.INTEGER,
        fromCache   = false,
        typeChanged = false,
        methodCall =
          Some(Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "inc")))
      ),
      TestMessages.update(
        contextId,
        idY,
        ConstantsGen.INTEGER,
        fromCache   = false,
        typeChanged = false,
        methodCall =
          Some(Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "inc")))
      ),
      TestMessages.update(
        contextId,
        idXSelfMain,
        moduleName,
        fromCache   = true,
        typeChanged = false
      ),
      TestMessages.update(
        contextId,
        idYSelfMain,
        moduleName,
        fromCache   = true,
        typeChanged = false
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("16")
  }

}
