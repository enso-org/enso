package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.instrument.execution.Timer
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

  // === Test Timer ===========================================================

  class TestTimer extends Timer {
    override def getTime(): Long = 0
  }

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

    val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[org.enso.interpreter.runtime.Context]
    languageContext.getLanguage.getIdExecutionService.ifPresent(
      _.overrideTimer(new TestTimer)
    );

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
    context.receiveN(6) should contain theSameElementsAs Seq(
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
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `undefined` could not be found.",
          Seq(xId)
        )
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
    context.receiveN(4) should contain theSameElementsAs Seq(
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
        Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo"),
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `x` could not be found.",
          Seq(mainBodyId)
        )
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
    val xId       = metadata.addItem(93, 19)
    val yId       = metadata.addItem(121, 8)
    val mainResId = metadata.addItem(134, 7)

    val code =
      """from Standard.Base.Error.Common import all
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
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
    val xId        = metadata.addItem(40, 9)
    val yId        = metadata.addItem(58, 2)
    val mainResId  = metadata.addItem(65, 12)

    val code =
      """import Standard.Base.IO
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
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
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
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
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
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, xId, ConstantsGen.INTEGER),
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
    context.receiveN(3) should contain theSameElementsAs Seq(
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
      "(Error: (Arithmetic_Error_Data 'Cannot divide by zero.'))"
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
    context.receiveN(3) should contain theSameElementsAs Seq(
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
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
    context.receiveN(1) should contain theSameElementsAs Seq(
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodPointer(moduleName, moduleName, "foo"),
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
    context.receiveN(1) should contain theSameElementsAs Seq(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("(Error: MyError2)")
  }

  it should "continue execution after thrown panics" in {
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError",
          Seq(xId)
        )
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

    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
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
    val xId        = metadata.addItem(49, 7)
    val yId        = metadata.addItem(65, 5)
    val mainResId  = metadata.addItem(75, 12)

    val code =
      """from Standard.Base.IO import all
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
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
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
    context.receiveN(4) should contain theSameElementsAs Seq(
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError1",
          Seq(xId)
        )
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      TestMessages.panic(
        contextId,
        xId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError2",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        yId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError2",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "MyError2",
          Seq(xId)
        )
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        xId,
        Api.MethodPointer(moduleName, moduleName, "foo"),
        Api.ExpressionUpdate.Payload.Panic(
          "9 (Integer)",
          Seq(xId)
        )
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        xId,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      TestMessages.update(contextId, mainResId, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3")
  }

  it should "send updates when dataflow error is resolved in method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val xId        = metadata.addItem(108, 3)
    val yId        = metadata.addItem(120, 5)
    val mainResId  = metadata.addItem(130, 12)

    val code =
      """import Standard.Base.IO
        |from Standard.Base.Error.Common import all
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
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        xId,
        Api.MethodPointer(moduleName, moduleName, "foo"),
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
              model.Range(model.Position(4, 4), model.Position(4, 17)),
              "10002 - 10000"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        xId,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      TestMessages.update(contextId, yId, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3")
  }

  it should "not cache panics" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val newline    = System.lineSeparator()

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
    context.receiveN(5) should contain theSameElementsAs Seq(
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
          "Compile error: The name `IO` could not be found.",
          Seq(xId)
        )
      ),
      TestMessages.panic(
        contextId,
        mainResId,
        Api.ExpressionUpdate.Payload.Panic(
          "Compile error: The name `IO` could not be found.",
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
              model.Range(model.Position(0, 0), model.Position(0, 0)),
              s"import Standard.Base.IO$newline$newline"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, x1Id, ConstantsGen.NOTHING),
      TestMessages.update(contextId, mainRes1Id, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("MyError")
  }

}
