package org.enso.interpreter.test.instrument

import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.polyglot.{
  LanguageInfo,
  RuntimeOptions,
  RuntimeServerInfo,
  Suggestion
}
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.logging.Level

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeTextEditsTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

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
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName)
        .option(
          RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION,
          "false"
        )
        .option(
          RuntimeOptions.INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION,
          "true"
        )
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
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

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    context.init()
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive

    context.send(
      Api.Request(UUID.randomUUID(), Api.StartBackgroundProcessing())
    )
    context.receive shouldEqual Some(
      Api.Response(Api.BackgroundJobsStartedNotification())
    )
  }

  override protected def afterEach(): Unit = {
    if (context != null) {
      context.close()
      context.out.reset()
      context = null
    }
  }

  it should "send accept multiple file modification" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val code =
      """from Standard.Base import all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val mainFile = context.writeMain(code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, code))
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
    context.receiveNIgnoreExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 19), model.Position(2, 31)),
              "Meh"
            )
          ),
          execute = false
        )
      )
    )
    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 19), model.Position(2, 22)),
              "Welcome!"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      1
    ) should contain theSameElementsAs Seq(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Welcome!")
  }
}
