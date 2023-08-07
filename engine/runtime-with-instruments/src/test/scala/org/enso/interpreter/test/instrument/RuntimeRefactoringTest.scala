package org.enso.interpreter.test.instrument

import org.apache.commons.io.output.TeeOutputStream
import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.{ContentBasedVersioning, Sha3_224VersionCalculator}
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeRefactoringTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  // === Test Utilities =======================================================

  var context: TestContext = _

  class TestContext(packageName: String) extends InstrumentTestContext {

    val tmpDir: Path = Files.createTempDirectory("enso-test-packages")
    sys.addShutdownHook(FileSystem.removeDirectoryIfExists(tmpDir))
    val lockManager = new ThreadSafeFileLockManager(tmpDir.resolve("locks"))
    val runtimeServerEmulator =
      new RuntimeServerEmulator(messageQueue, lockManager)

    val pkg: Package[File] =
      PackageManager.Default.create(tmpDir.toFile, packageName, "Enso_Test")
    val out: ByteArrayOutputStream    = new ByteArrayOutputStream()
    val logOut: ByteArrayOutputStream = new ByteArrayOutputStream()
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.LOG_LEVEL, "WARNING")
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
        .logHandler(new TeeOutputStream(logOut, System.err))
        .out(new TeeOutputStream(out, System.err))
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    def writeMain(contents: String): File =
      Files.write(pkg.mainFile.toPath, contents.getBytes).toFile

    def readMain: String =
      Files.readString(pkg.mainFile.toPath)

    def send(msg: Api.Request): Unit = runtimeServerEmulator.sendToRuntime(msg)

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

    def executionComplete(contextId: UUID): Api.Response =
      Api.Response(Api.ExecutionComplete(contextId))
  }

  val versionCalculator: ContentBasedVersioning = Sha3_224VersionCalculator

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  override protected def afterEach(): Unit = {
    context.executionContext.context.close()
    context.runtimeServerEmulator.terminate()
  }

  "RuntimeServer" should "rename operator in main body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idOperator1 = metadata.addItem(11, 9)
    val code =
      """main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    operator2
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
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

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

    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()

    // rename operator1
    val newName = "foobar"
    val edits = Vector(
      TextEdit(
        model.Range(model.Position(1, 4), model.Position(1, 13)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(2, 16), model.Position(2, 25)),
        newName
      )
    )
    val fileEdit = Api.FileEdit(
      context.pkg.mainFile,
      edits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("operator1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idOperator1, newName))
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, fileEdit),
      context.executionComplete(contextId)
    )
  }

  it should "edit file after renaming" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata        = new Metadata
    val symbolOperator1 = metadata.addItem(11, 9)
    val exprOperator1   = metadata.addItem(23, 2)
    val exprOperator2   = metadata.addItem(42, 13)
    val code =
      """main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    operator2
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
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

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

    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages
        .update(contextId, exprOperator1, ConstantsGen.INTEGER_BUILTIN),
      TestMessages
        .update(contextId, exprOperator2, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()

    // rename operator1
    val newName = "foobar"
    val edits = Vector(
      TextEdit(
        model.Range(model.Position(1, 4), model.Position(1, 13)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(2, 16), model.Position(2, 25)),
        newName
      )
    )
    val fileEdit = Api.FileEdit(
      context.pkg.mainFile,
      edits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("operator1", newName))
        .toHexString
    )
    context.send(
      Api.Request(
        requestId,
        Api.RenameSymbol(moduleName, symbolOperator1, newName)
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, fileEdit),
      context.executionComplete(contextId)
    )

    // modify main
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 16), model.Position(1, 18)),
              "42"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, exprOperator1, exprOperator2),
      TestMessages
        .update(
          contextId,
          exprOperator1,
          ConstantsGen.INTEGER_BUILTIN,
          typeChanged = false
        ),
      TestMessages
        .update(
          contextId,
          exprOperator2,
          ConstantsGen.INTEGER_BUILTIN,
          typeChanged = false
        ),
      context.executionComplete(contextId)
    )

  }

}
