package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.enso.text.{ContentVersion, Sha3_224VersionCalculator}
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeErrorsTest
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
      PackageManager.Default.create(
        tmpDir.toFile,
        packageName,
        namespace = "Enso_Test"
      )
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
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
        .out(out)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

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
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  override protected def afterEach(): Unit = {
    context.executionContext.context.close()
    context.runtimeServerEmulator.terminate()
  }

  it should "return panic sentinels in method body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    // foo body id
    metadata.addItem(21, 5)
    val xId       = metadata.addItem(35, 9)
    val yId       = metadata.addItem(53, 8)
    val mainResId = metadata.addItem(66, 7)

    val code =
      """main =
        |    foo a b = a + b
        |    x = undefined
        |    y = foo x 42
        |    foo y 1
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      7
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `undefined` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(2, 8), model.Position(2, 17))),
              Some(xId)
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile_Error.Error",
          Seq(xId)
        ),
        builtin = true
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile_Error.Error",
          Seq(xId)
        ),
        builtin = true
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile_Error.Error",
          Seq(xId)
        ),
        builtin = true
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return panic sentinels in method body (pretty print)" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    // foo body id
    metadata.addItem(65, 5)
    val xId       = metadata.addItem(79, 9)
    val yId       = metadata.addItem(97, 8)
    val mainResId = metadata.addItem(110, 7)

    val code =
      """from Standard.Base.Errors.Common import all
        |main =
        |    foo a b = a + b
        |    x = undefined
        |    y = foo x 42
        |    foo y 1
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      7
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `undefined` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 8), model.Position(3, 17))),
              Some(xId)
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        ),
        builtin = true
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        ),
        builtin = true
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        ),
        builtin = true
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return panic sentinels in method calls" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val mainBodyId = metadata.addItem(28, 7)

    val code =
      """foo a b = a + b + x
        |
        |main = foo 1 2
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `x` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(0, 18), model.Position(0, 19))),
              None
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        mainBodyId,
        Api.MethodCall(
          Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo")
        ),
        Api.ExpressionUpdate.Payload.Panic(
          "Compile_Error.Error",
          Seq(mainBodyId)
        ),
        builtin = true
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return panic sentinels in method calls (pretty print)" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val mainBodyId = metadata.addItem(72, 7)

    val code =
      """from Standard.Base.Errors.Common import all
        |foo a b = a + b + x
        |
        |main = foo 1 2
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `x` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(1, 18), model.Position(1, 19))),
              None
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        mainBodyId,
        Api.MethodCall(
          Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo")
        ),
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `x` could not be found.",
          Seq(mainBodyId)
        ),
        builtin = true
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return dataflow errors in method body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    // foo body id
    metadata.addItem(79, 5)
    val xId       = metadata.addItem(83, 19)
    val yId       = metadata.addItem(111, 8)
    val mainResId = metadata.addItem(124, 7)

    val code =
      """import Standard.Base.Error.Error
        |
        |type MyError
        |
        |main =
        |    foo a b = a + b
        |    x = Error.throw MyError
        |    y = foo x 42
        |    foo y 1
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Error",
            "Standard.Base.Error.Error",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return panic sentinels continuing execution" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(46, 9)
    val yId        = metadata.addItem(64, 2)
    val mainResId  = metadata.addItem(71, 12)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = undefined
        |    y = 42
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      7
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.warning(
              "Unused variable x.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 4), model.Position(3, 5)))
            ),
            Api.ExecutionResult.Diagnostic.error(
              "The name `undefined` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 8), model.Position(3, 17))),
              Some(xId)
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        )
      ),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq("42")
  }

  it should "return dataflow errors continuing execution" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(60, 19)
    val yId        = metadata.addItem(88, 2)
    val mainResId  = metadata.addItem(95, 12)

    val code =
      """from Standard.Base import all
        |
        |type MyError
        |
        |main =
        |    x = Error.throw MyError
        |    y = 42
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      7
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.warning(
              "Unused variable x.",
              Some(mainFile),
              Some(model.Range(model.Position(5, 4), model.Position(5, 5)))
            )
          )
        )
      ),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Error",
            "Standard.Base.Error.Error",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq("42")
  }

  it should "continue execution after dataflow errors" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(60, 19)
    val yId        = metadata.addItem(88, 5)
    val mainResId  = metadata.addItem(98, 12)

    val code =
      """from Standard.Base import all
        |
        |type MyError
        |
        |main =
        |    x = Error.throw MyError
        |    y = x - 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Error",
            "Standard.Base.Error.Error",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq("(Error: MyError)")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 8), model.Position(5, 27)),
              "1234567890123456789"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3,
      updatesOnlyFor = Set(xId, yId)
    ) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        xId,
        ConstantsGen.INTEGER,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Error",
              "Standard.Base.Error.Error",
              "throw"
            )
          )
        )
      ),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("1234567890123456788")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 8), model.Position(5, 27)),
              "1000000000000.div 0"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3,
      updatesOnlyFor = Set(xId, yId)
    ) should contain theSameElementsAs Seq(
      TestMessages.error(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List(
      "(Error: (Arithmetic_Error.Error 'Cannot divide by zero.'))"
    )

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 8), model.Position(5, 27)),
              "1000000000000.div 2"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3,
      updatesOnlyFor = Set(xId, yId)
    ) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, xId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("499999999999")
  }

  it should "not send updates when dataflow error changes in expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(75, 20)
    val yId        = metadata.addItem(104, 5)
    val mainResId  = metadata.addItem(114, 12)

    val code =
      """from Standard.Base import all
        |
        |type MyError1
        |type MyError2
        |
        |main =
        |    x = Error.throw MyError1
        |    y = x - 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Error",
            "Standard.Base.Error.Error",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq("(Error: MyError1)")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(6, 20), model.Position(6, 28)),
              "MyError2"
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
    context.consumeOut shouldEqual List("(Error: MyError2)")
  }

  it should "not send updates when dataflow error changes in method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val fooThrowId = metadata.addItem(70, 20)
    val xId        = metadata.addItem(107, 3)
    val yId        = metadata.addItem(119, 5)
    val mainResId  = metadata.addItem(129, 12)

    val code =
      """from Standard.Base import all
        |
        |type MyError1
        |type MyError2
        |
        |foo =
        |    Error.throw MyError1
        |
        |main =
        |    x = foo
        |    y = x - 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "foo")),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(fooThrowId, xId))
      ),
      TestMessages.error(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(fooThrowId, xId))
      ),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq("(Error: MyError1)")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(6, 16), model.Position(6, 24)),
              "MyError2"
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
    context.consumeOut shouldEqual List("(Error: MyError2)")
  }

  it should "return dataflow errors over warnings" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(46, 9)
    val yId        = metadata.addItem(64, 72)
    val mainResId  = metadata.addItem(141, 7)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = [1, 2, 3]
        |    y = Warning.attach_with_stacktrace x "foo" Runtime.primitive_get_stack_trace
        |    y.at 10
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xId, ConstantsGen.VECTOR),
      TestMessages.update(
        contextId,
        yId,
        ConstantsGen.VECTOR,
        payload = Api.ExpressionUpdate.Payload.Value(
          Some(
            Api.ExpressionUpdate.Payload.Value.Warnings(1, Some("'foo'"), false)
          )
        )
      ),
      TestMessages.error(
        contextId,
        mainResId,
        methodCall = Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Data.Vector",
            "Standard.Base.Data.Vector.Vector",
            "at"
          )
        ),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(mainResId))
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()
  }

  it should "continue execution after thrown panics" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(60, 19, "aa")
    val yId        = metadata.addItem(88, 5)
    val mainResId  = metadata.addItem(98, 12)

    val code =
      """from Standard.Base import all
        |
        |type MyError
        |
        |main =
        |    x = Panic.throw MyError
        |    y = x - 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Panic",
            "Standard.Base.Panic.Panic",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.Panic(
          "MyError",
          Seq(xId)
        ),
        Some("Standard.Base.Panic.Panic")
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError",
          Seq(xId)
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 8), model.Position(5, 27)),
              "1234567890123456789"
            )
          ),
          execute = true
        )
      )
    )

    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, xId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("1234567890123456788")
  }

  it should "continue execution after panics in expressions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(46, 7)
    val yId        = metadata.addItem(62, 5)
    val mainResId  = metadata.addItem(72, 12)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = 1 + foo
        |    y = x - 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      7
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `foo` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 12), model.Position(3, 15))),
              None
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `foo` could not be found.",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `foo` could not be found.",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `foo` could not be found.",
          Seq(xId)
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 12), model.Position(3, 15)),
              "101"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, xId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("101")

  }

  it should "send updates when panic changes in expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(75, 20)
    val yId        = metadata.addItem(104, 5)
    val mainResId  = metadata.addItem(114, 12)

    val code =
      """from Standard.Base import all
        |
        |type MyError1
        |type MyError2
        |
        |main =
        |    x = Panic.throw MyError1
        |    y = x - 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Panic",
            "Standard.Base.Panic.Panic",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.Panic(
          "MyError1",
          Seq(xId)
        ),
        Some("Standard.Base.Panic.Panic")
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError1",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError1",
          Seq(xId)
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(6, 20), model.Position(6, 28)),
              "MyError2"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      TestMessages.panic(
        contextId,
        xId,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Panic",
            "Standard.Base.Panic.Panic",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.Panic(
          "MyError2",
          Seq(xId)
        ),
        Some("Standard.Base.Panic.Panic")
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError2",
          Seq(xId)
        ),
        builtin     = false,
        typeChanged = false
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError2",
          Seq(xId)
        ),
        builtin = false
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()
  }

  it should "send updates when panic is resolved in method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(71, 3)
    val yId        = metadata.addItem(83, 5)
    val mainResId  = metadata.addItem(93, 12)

    val code =
      """from Standard.Base import all
        |
        |foo =
        |    Panic.throw 9
        |
        |main =
        |    x = foo
        |    y = x + 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "foo")),
        Api.ExpressionUpdate.Payload.Panic(
          "9 (Integer)",
          Seq(xId)
        ),
        builtin = false
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "9 (Integer)",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "9 (Integer)",
          Seq(xId)
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 4), model.Position(3, 17)),
              "10002 - 10000"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        xId,
        ConstantsGen.INTEGER,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "foo")),
        fromCache   = false,
        typeChanged = true
      ),
      TestMessages
        .update(contextId, yId, ConstantsGen.INTEGER, typeChanged = true),
      TestMessages
        .update(contextId, mainResId, ConstantsGen.NOTHING, typeChanged = true),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3")
  }

  it should "send updates when NPE is resolved in method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(146, 3, "aaa")

    val code =
      """from Standard.Base import all
        |polyglot java import java.lang.NullPointerException
        |
        |foo =
        |    Panic.throw NullPointerException.new
        |
        |main =
        |    x = foo
        |    x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    metadata.assertInCode(xId, code, "foo")

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "foo")),
        Api.ExpressionUpdate.Payload.Panic(
          "java.lang.NullPointerException",
          Seq(xId)
        ),
        None
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()
  }

  it should "send updates when dataflow error is resolved in method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(71, 3)
    val yId        = metadata.addItem(83, 5)
    val mainResId  = metadata.addItem(93, 12)

    val code =
      """from Standard.Base import all
        |
        |foo =
        |    Error.throw 9
        |
        |main =
        |    x = foo
        |    y = x + 1
        |    IO.println y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "foo")),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.update(
        contextId,
        mainResId,
        ConstantsGen.NOTHING
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq("(Error: 9)")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 4), model.Position(3, 17)),
              "10002 - 10000"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3,
      updatesOnlyFor = Set(xId, yId)
    ) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        xId,
        ConstantsGen.INTEGER,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "foo")),
        fromCache   = false,
        typeChanged = true
      ),
      TestMessages
        .update(contextId, yId, ConstantsGen.INTEGER, typeChanged = true),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3")
  }

  it should "not cache panics" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val newline    = "\n" // was: System.lineSeparator()

    val metadata   = new Metadata
    val xId        = metadata.addItem(15, 20)
    val mainResId  = metadata.addItem(40, 1)
    val x1Id       = metadata.addItem(40, 20)
    val mainRes1Id = metadata.addItem(65, 1)

    val code =
      """main =
        |    x = IO.println "MyError"
        |    x
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `IO` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(1, 8), model.Position(1, 10))),
              None
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile_Error.Error",
          Seq(xId)
        ),
        builtin = true
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile_Error.Error",
          Seq(xId)
        ),
        builtin = true
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 0), model.Position(0, 0)),
              s"import Standard.Base.IO$newline$newline"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, x1Id, ConstantsGen.NOTHING_BUILTIN),
      TestMessages.update(contextId, mainRes1Id, ConstantsGen.NOTHING_BUILTIN),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("MyError")
  }

  it should "not cache panics (pretty print)" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val newline    = "\n" // was: System.lineSeparator()

    val metadata   = new Metadata
    val xId        = metadata.addItem(59, 20)
    val mainResId  = metadata.addItem(84, 1)
    val x1Id       = metadata.addItem(84, 20)
    val mainRes1Id = metadata.addItem(109, 1)

    val code =
      """from Standard.Base.Errors.Common import all
        |main =
        |    x = IO.println "MyError"
        |    x
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "The name `IO` could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(2, 8), model.Position(2, 10))),
              None
            )
          )
        )
      ),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `IO` could not be found.",
          Seq(xId)
        ),
        builtin = true
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `IO` could not be found.",
          Seq(xId)
        ),
        builtin = true
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 0), model.Position(0, 0)),
              s"import Standard.Base.IO$newline$newline"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, x1Id, ConstantsGen.NOTHING),
      TestMessages.update(contextId, mainRes1Id, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("MyError")
  }

  it should "cache dataflow errors" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val newline    = "\n" // was: System.lineSeparator()

    val metadata   = new Metadata
    val xId        = metadata.addItem(111, 79)
    val mainResId  = metadata.addItem(195, 1)
    val mainRes1Id = metadata.addItem(209, 1)

    val code =
      """import Standard.Base.Error.Error
        |import Standard.Base.Errors.Illegal_Argument.Illegal_Argument
        |
        |main =
        |    x = Error.throw (Illegal_Argument.Error "The operation failed due to some reason.")
        |    x
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        methodCall = Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Error",
            "Standard.Base.Error.Error",
            "throw"
          )
        ),
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      TestMessages.error(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 4), model.Position(5, 5)),
              s"y = x - 1${newline}    y"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      TestMessages.error(
        contextId,
        mainRes1Id,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(xId))
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual Seq()
  }

  it should "not return cached method pointer when node panics" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val operator1Id = metadata.addItem(54, 17)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = Main.function1 42
        |    operator1
        |
        |function1 x = x
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        operator1Id,
        ConstantsGen.INTEGER,
        Api.MethodCall(
          Api.MethodPointer(moduleName, moduleName, "function1")
        )
      ),
      context.executionComplete(contextId)
    )

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 29), model.Position(3, 30)),
              "2"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, operator1Id),
      TestMessages.panic(
        contextId,
        operator1Id,
        Api.ExpressionUpdate.Payload.Panic(
          "Method `function2` of type Main could not be found.",
          Seq(operator1Id)
        )
      ),
      context.executionComplete(contextId)
    )
  }

}
