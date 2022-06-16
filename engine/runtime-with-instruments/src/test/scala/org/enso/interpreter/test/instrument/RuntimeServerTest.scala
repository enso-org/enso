package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.instrument.execution.Timer
import org.enso.interpreter.runtime.`type`.{ConstantsGen, Types}
import org.enso.interpreter.runtime.{Context => EnsoContext}
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.data.TypeGraph
import org.enso.polyglot.runtime.Runtime.Api
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
class RuntimeServerTest
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
        .logHandler(logOut)
        .out(out)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[EnsoContext]
    val info =
      languageContext.getEnvironment.getPublicLanguages.get(LanguageInfo.ID)
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

    // === The Tests ==========================================================

    object Main {

      val metadata = new Metadata

      val idMainX = metadata.addItem(63, 1)
      val idMainY = metadata.addItem(73, 7)
      val idMainZ = metadata.addItem(89, 5)
      val idFooY  = metadata.addItem(128, 8)
      val idFooZ  = metadata.addItem(145, 5)

      def code =
        metadata.appendToCode(
          """
            |from Standard.Base.Data.Numbers import Number
            |
            |main =
            |    x = 6
            |    y = x.foo 5
            |    z = y + 5
            |    z
            |
            |Number.foo = x ->
            |    y = this + 3
            |    z = y * x
            |    z
            |""".stripMargin.linesIterator.mkString("\n")
        )

      object Update {

        def mainX(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idMainX,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def mainY(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idMainY,
                  Some(ConstantsGen.INTEGER),
                  Some(
                    Api.MethodPointer(
                      "Enso_Test.Test.Main",
                      ConstantsGen.NUMBER,
                      "foo"
                    )
                  ),
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def mainZ(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idMainZ,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def fooY(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idFooY,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def fooZ(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idFooZ,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )
      }
    }

    object Main2 {

      val metadata = new Metadata
      val idMainY  = metadata.addItem(173, 10)
      val idMainZ  = metadata.addItem(192, 10)

      val code = metadata.appendToCode(
        """
          |import Standard.Base.IO
          |
          |foo = arg ->
          |    IO.println "I'm expensive!"
          |    arg + 5
          |
          |bar = arg ->
          |    IO.println "I'm more expensive!"
          |    arg * 5
          |
          |main =
          |    x = 10
          |    y = here.foo x
          |    z = here.bar y
          |    z
          |""".stripMargin
      )

      object Update {

        def mainY(contextId: UUID) =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  idMainY,
                  Some(ConstantsGen.INTEGER),
                  Some(
                    Api.MethodPointer(
                      "Enso_Test.Test.Main",
                      "Enso_Test.Test.Main",
                      "foo"
                    )
                  ),
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  false,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def mainZ(contextId: UUID) =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  idMainZ,
                  Some(ConstantsGen.INTEGER),
                  Some(
                    Api.MethodPointer(
                      "Enso_Test.Test.Main",
                      "Enso_Test.Test.Main",
                      "bar"
                    )
                  ),
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  false,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )
      }
    }

    object Visualisation {

      val code =
        """
          |encode = x -> x.to_text
          |
          |incAndEncode = x -> here.encode x+1
          |
          |""".stripMargin

    }

  }

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  "RuntimeServer" should "push and pop functions on the stack" in {
    val contents  = context.Main.code
    val mainFile  = context.writeMain(contents)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

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

    // push local item on top of the empty stack
    val invalidLocalItem = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api
        .Request(requestId, Api.PushContextRequest(contextId, invalidLocalItem))
    )
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.InvalidStackItemError(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionComplete(contextId)
    )

    // push method pointer on top of the non-empty stack
    val invalidExplicitCall = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, invalidExplicitCall)
      )
    )
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.InvalidStackItemError(contextId))
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      context.Main.Update.mainY(contextId, fromCache = true),
      context.executionComplete(contextId)
    )

    // pop main
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.PopContextResponse(contextId))
    )

    // pop empty stack
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.EmptyStackError(contextId))
    )
  }

  it should "push method with default arguments" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(48, 24)
    val idMainFoo = metadata.addItem(64, 8)

    val code =
      """import Standard.Base.IO
        |
        |foo a=0 = a + 1
        |
        |main =
        |    IO.println here.foo
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
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
        idMainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "foo")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("1")

    // push foo call
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(idMainFoo))
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("1")
  }

  it should "send method pointer updates" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(103, 121)
    val idMainX  = metadata.addItem(130, 8)
    val idMainY  = metadata.addItem(147, 8)
    val idMainM  = metadata.addItem(164, 5)
    val idMainP  = metadata.addItem(178, 5)
    val idMainQ  = metadata.addItem(192, 5)
    val idMainF  = metadata.addItem(214, 9)

    val code =
      """import Standard.Base.IO
        |import Enso_Test.Test.A
        |
        |type Quux
        |    type Quux
        |
        |    foo = 42
        |
        |bar = 7
        |
        |main =
        |    f a b = a + b
        |    x = Quux.foo
        |    y = here.bar
        |    m = A.A x
        |    p = m.foo
        |    q = A.bar
        |    IO.println (f x+y p+q)
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    val aCode =
      """
        |type A
        |    type A un_a
        |
        |    foo = 11
        |
        |bar = 19
        |""".stripMargin
    val aFile = context.writeInSrcDir("A", aCode)

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
    context.send(Api.Request(Api.OpenFileNotification(aFile, aCode)))
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
    context.receiveNIgnoreStdLib(9) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainX,
        ConstantsGen.INTEGER,
        Api.MethodPointer(
          "Enso_Test.Test.Main",
          "Enso_Test.Test.Main.Quux",
          "foo"
        )
      ),
      TestMessages.update(
        contextId,
        idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "bar")
      ),
      TestMessages.update(contextId, idMainM, "Enso_Test.Test.A.A"),
      TestMessages.update(
        contextId,
        idMainP,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Test.A", "Enso_Test.Test.A.A", "foo")
      ),
      TestMessages.update(
        contextId,
        idMainQ,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Test.A", "Enso_Test.Test.A", "bar")
      ),
      TestMessages.update(contextId, idMainF, ConstantsGen.INTEGER),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("79")
  }

  it should "send updates from last line" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(23, 17)
    val idMainFoo = metadata.addItem(28, 12)

    val code =
      """foo a b = a + b
        |
        |main =
        |    this.foo 1 2
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "foo")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "compute side effects correctly from last line" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(48, 30)
    val idMainFoo = metadata.addItem(65, 12)

    val code =
      """import Standard.Base.IO
        |
        |foo a b = a + b
        |
        |main =
        |    IO.println (this.foo 1 2)
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
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
        idMainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "foo")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3")
  }

  it should "run State getting the initial state" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(113, 41)
    val idMainBar = metadata.addItem(145, 8)

    val code =
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.IO
        |import Standard.Base.Runtime.State
        |
        |main = IO.println (State.run Number 42 this.bar)
        |
        |bar = State.get Number
        |""".stripMargin
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
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
        idMainBar,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "bar")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "run State setting the state" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(113, 40)
    val idMainBar = metadata.addItem(144, 8)

    val code =
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.IO
        |import Standard.Base.Runtime.State
        |
        |main = IO.println (State.run Number 0 this.bar)
        |
        |bar =
        |    State.put Number 10
        |    State.get Number
        |""".stripMargin
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
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
        idMainBar,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "bar")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("10")
  }

  it should "send updates of a function call" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(23, 23)
    val idMainFoo = metadata.addItem(28, 12)

    val code =
      """foo a b = a + b
        |
        |main =
        |    this.foo 1 2
        |    1
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "foo")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "send updates when function body is changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    // foo definition
    metadata.addItem(25, 22)
    // foo name
    metadata.addItem(25, 3)
    val fooX    = metadata.addItem(39, 1)
    val fooRes  = metadata.addItem(45, 1)
    val mainFoo = metadata.addItem(63, 8)
    val mainRes = metadata.addItem(76, 12)

    val code =
      """import Standard.Base.IO
        |
        |foo =
        |    x = 4
        |    x
        |
        |main =
        |    y = here.foo
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages
        .update(
          contextId,
          mainFoo,
          ConstantsGen.INTEGER,
          Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo")
        ),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("4")

    // push foo call
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(mainFoo))
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, fooX, ConstantsGen.INTEGER),
      TestMessages.update(contextId, fooRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("4")

    // Modify the foo method
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 8), model.Position(3, 9)),
              "5"
            )
          )
        )
      )
    )
    context.receiveN(1) should contain theSameElementsAs Seq(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("5")

    // pop the foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages
        .update(
          contextId,
          mainFoo,
          ConstantsGen.INTEGER,
          Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo")
        ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("5")
  }

  it should "not send updates when the type is not changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val idMain     = context.Main.metadata.addItem(54, 47)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)

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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionComplete(contextId)
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      context.Main.Update.mainY(contextId, fromCache = true),
      context.executionComplete(contextId)
    )

    // pop main
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId))
    )
  }

  it should "send updates when the type is changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idResult  = metadata.addItem(45, 4)
    val idPrintln = metadata.addItem(54, 17)
    val idMain    = metadata.addItem(31, 40)
    val code =
      """import Standard.Base.IO
        |
        |main =
        |    result = 1337
        |    IO.println result
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idResult, ConstantsGen.INTEGER),
      TestMessages.update(contextId, idPrintln, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("1337")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 13), model.Position(3, 17)),
              "\"Hi\""
            )
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, idResult, ConstantsGen.TEXT),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hi")
  }

  it should "send updates when the method pointer is changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(77, 35)
    val idMainA  = metadata.addItem(86, 8)
    val idMainP  = metadata.addItem(99, 12)
    // pie id
    metadata.addItem(119, 1)
    // uwu id
    metadata.addItem(127, 1)
    // hie id
    metadata.addItem(135, 6)
    // Number.x id
    metadata.addItem(155, 1)
    val code =
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.IO
        |
        |main =
        |    a = 123 + 21
        |    IO.println a
        |
        |pie = 3
        |uwu = 7
        |hie = "hie!"
        |Number.x y = y
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMainA, ConstantsGen.INTEGER),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("144")

    // Edit s/123 + 21/1234.x 4/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 16)),
              "1234.x 4"
            )
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "x")
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("4")

    // Edit s/1234.x 4/1000.x 5/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 16)),
              "1000.x 5"
            )
          )
        )
      )
    )
    context.receiveN(1) shouldEqual Seq(context.executionComplete(contextId))
    context.consumeOut shouldEqual List("5")

    // Edit s/1000.x 5/Main.pie/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 16)),
              "here.pie"
            )
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "pie")
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3")

    // Edit s/Main.pie/Main.uwu/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 16)),
              "here.uwu"
            )
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "uwu")
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("7")

    // Edit s/Main.uwu/Main.hie/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 16)),
              "here.hie"
            )
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.TEXT,
        Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "hie")
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("hie!")

    // Edit s/Main.hie/"Hello!"/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 16)),
              "\"Hello!\""
            )
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, idMainA, ConstantsGen.TEXT),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello!")
  }

  it should "send updates for overloaded functions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(122, 88)
    val id1      = metadata.addItem(131, 15)
    val id2      = metadata.addItem(151, 18)
    val id3      = metadata.addItem(174, 15)
    // Note that Nothing.Nothing is on purpose.
    // If not provided the full name it will resolve the expression Nothing to a Nothing module.
    // Similarly Text.Text. That in turn will mismatch the expectations for method types which actually
    // return proper types.
    val code =
      """from Standard.Base.Data.Numbers import Number
        |from Standard.Base.Data.Text import all
        |import Standard.Base.Nothing
        |
        |main =
        |    x = 15.overloaded 1
        |    "foo".overloaded 2
        |    10.overloaded x
        |    Nothing.Nothing
        |
        |Text.Text.overloaded arg = arg + 1
        |Number.overloaded arg = arg + 2
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
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      TestMessages.update(
        contextId,
        id1,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded")
      ),
      TestMessages.update(
        contextId,
        id2,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.TEXT, "overloaded")
      ),
      TestMessages.update(
        contextId,
        id3,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded")
      ),
      context.executionComplete(contextId)
    )

    // push call1
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.LocalCall(id1)
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    // pop call1
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(
        contextId,
        id1,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded"),
        fromCache = true
      ),
      TestMessages.update(
        contextId,
        id2,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.TEXT, "overloaded")
      ),
      TestMessages.update(
        contextId,
        id3,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded")
      ),
      context.executionComplete(contextId)
    )

    // push call2
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.LocalCall(id2)
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    // pop call2
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(
        contextId,
        id1,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded"),
        fromCache = true
      ),
      TestMessages.update(
        contextId,
        id2,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.TEXT, "overloaded")
      ),
      TestMessages.update(
        contextId,
        id3,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded")
      ),
      context.executionComplete(contextId)
    )

    // push call3
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.LocalCall(id3)
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    // pop call3
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(
        contextId,
        id1,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded"),
        fromCache = true
      ),
      TestMessages.update(
        contextId,
        id2,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.TEXT, "overloaded")
      ),
      TestMessages.update(
        contextId,
        id3,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded")
      ),
      context.executionComplete(contextId)
    )
  }

  it should "send updates for a lambda" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val xId     = metadata.addItem(40, 10)
    val mainRes = metadata.addItem(55, 12)

    val code =
      """import Standard.Base.IO
        |
        |main =
        |    x = a -> a + 1
        |    IO.println x
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
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xId, ConstantsGen.FUNCTION),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
  }

  it should "support file modification operations without attached ids" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val moduleName = "Enso_Test.Test.Main"
    val code =
      """import Standard.Base.IO
        |
        |main = IO.println "I'm a file!"
        |""".stripMargin

    // Create a new file
    val mainFile = context.writeMain(code)

    // Open the new file
    context.send(Api.Request(Api.OpenFileNotification(mainFile, code)))
    context.receiveNone shouldEqual None
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm a file!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 25), model.Position(2, 29)),
              "modified"
            )
          )
        )
      )
    )
    context.receive shouldEqual Some(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm a modified!")

    // Close the file
    context.send(Api.Request(Api.CloseFileNotification(mainFile)))
    context.consumeOut shouldEqual List()
  }

  it should "support file modification operations with attached ids" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val idMain     = metadata.addItem(7, 2)
    val code       = metadata.appendToCode("main = 84")

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Create a new file
    val mainFile = context.writeMain(code)

    // Open the new file
    context.send(Api.Request(Api.OpenFileNotification(mainFile, code)))
    context.receiveNone shouldEqual None

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 0), model.Position(0, 9)),
              "main = 42"
            )
          )
        )
      )
    )
    context.receive shouldEqual Some(
      context.executionComplete(contextId)
    )
  }

  it should "send suggestion notifications when file is executed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val idMain     = context.Main.metadata.addItem(54, 47)

    val mainFile = context.writeMain(context.Main.code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, context.Main.code))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionComplete(contextId)
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      context.Main.Update.mainY(contextId, fromCache = true),
      context.executionComplete(contextId)
    )

    // pop main
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId))
    )

    // pop empty stack
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.EmptyStackError(contextId))
    )
  }

  it should "send suggestion notifications when file is modified" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val newline    = System.lineSeparator()

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val code =
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.IO
        |
        |main = IO.println "I'm a file!"
        |""".stripMargin

    // Create a new file
    val mainFile = context.writeMain(code)

    // Open the new file
    context.send(Api.Request(Api.OpenFileNotification(mainFile, code)))
    context.receiveNone shouldEqual None
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm a file!")

    /*
      Modify the file:
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.IO
        |
        |Number.lucky = 42
        |
        |main = IO.println "I'm a modified!"
        |""".stripMargin
     */
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 25), model.Position(3, 29)),
              "modified"
            ),
            TextEdit(
              model.Range(model.Position(3, 0), model.Position(3, 0)),
              s"Number.lucky = 42$newline$newline"
            )
          )
        )
      )
    )
    context.receiveN(1) should contain theSameElementsAs Seq(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm a modified!")

    // Close the file
    context.send(Api.Request(Api.CloseFileNotification(mainFile)))
    context.receiveNone shouldEqual None
    context.consumeOut shouldEqual List()
  }

  it should "recompute expressions without invalidation" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

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
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
  }

  it should "recompute expressions invalidating all" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

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
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(Api.InvalidatedExpressions.All())
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
  }

  it should "recompute expressions invalidating some" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

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
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(
            Api.InvalidatedExpressions.Expressions(Vector(context.Main.idMainZ))
          )
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
  }

  it should "return error when module not found" in {
    val contents  = context.Main.code
    val mainFile  = context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

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
            Api.MethodPointer("Unnamed.Main", "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          Api.ExecutionResult.Failure("Module Unnamed.Main not found.", None)
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error when constructor not found" in {
    val contents  = context.Main.code
    val mainFile  = context.writeMain(contents)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

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
            Api.MethodPointer(
              "Enso_Test.Test.Main",
              "Enso_Test.Test.Unexpected",
              "main"
            ),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          Api.ExecutionResult.Failure(
            "Constructor Unexpected not found in module Enso_Test.Test.Main.",
            Some(mainFile)
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error when method not found" in {
    val contents  = context.Main.code
    val mainFile  = context.writeMain(contents)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

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
            Api.MethodPointer(
              "Enso_Test.Test.Main",
              "Enso_Test.Test.Main",
              "ooops"
            ),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          Api.ExecutionResult.Failure(
            "Object Main does not define method ooops in module Enso_Test.Test.Main.",
            Some(mainFile)
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error not invocable" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val code =
      """main = this.bar 40 2 123
        |
        |bar x y = x + y
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Type error: expected a function, but got 42 (Integer).",
              Some(mainFile),
              Some(model.Range(model.Position(0, 7), model.Position(0, 24))),
              None,
              Vector(
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(0, 7), model.Position(0, 24))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error unresolved symbol" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val code =
      """main = this.bar .x .y
        |
        |bar one two = one + two
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Method `+` of x (Unresolved_Symbol) could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(2, 14), model.Position(2, 23))),
              None,
              Vector(
                Api.StackTraceElement(
                  "Main.bar",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(2, 14), model.Position(2, 23))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(0, 7), model.Position(0, 21))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error unexpected type" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata
    val code =
      """main = this.bar "one" 2
        |
        |bar x y = x + y
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Type error: expected `str` to be Text, but got 2 (Integer).",
              None,
              None,
              None,
              Vector(
                Api.StackTraceElement("Text.+", None, None, None),
                Api.StackTraceElement(
                  "Main.bar",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(2, 10), model.Position(2, 15))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(0, 7), model.Position(0, 23))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error method does not exist" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """from Standard.Base.Data.Numbers import Number
        |
        |main = Number.pi
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
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Method `pi` of Number could not be found.",
              Some(mainFile),
              Some(model.Range(model.Position(2, 7), model.Position(2, 16))),
              None,
              Vector(
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(2, 7), model.Position(2, 16))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error when method name clashes with atom" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """type Foo
        |foo = 0
        |main = 0
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Method definitions with the same name as atoms are not supported. Method foo clashes with the atom Foo in this module.",
              Some(mainFile),
              Some(model.Range(model.Position(1, 0), model.Position(1, 7))),
              None,
              Vector()
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return error with a stack trace" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """main =
        |    this.foo
        |
        |foo =
        |    x = this.bar
        |    x
        |bar =
        |    x = this.baz
        |    x
        |baz =
        |    x = 1 + .quux
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Type error: expected `that` to be Number, but got quux (Unresolved_Symbol).",
              None,
              None,
              None,
              Vector(
                Api.StackTraceElement("Small_Integer.+", None, None, None),
                Api.StackTraceElement(
                  "Main.baz",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(10, 8), model.Position(10, 17))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.bar",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(7, 8), model.Position(7, 16))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.foo",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(4, 8), model.Position(4, 16))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(1, 4), model.Position(1, 12))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return compiler warning unused variable" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """main =
        |    x = 1
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.warning(
              "Unused variable x.",
              Some(mainFile),
              Some(model.Range(model.Position(1, 4), model.Position(1, 5))),
              None
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return compiler warning unused argument" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """foo x = 1
        |
        |main = 42
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.warning(
              "Unused function argument x.",
              Some(mainFile),
              Some(model.Range(model.Position(0, 4), model.Position(0, 5)))
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return compiler error variable redefined" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """main =
        |    x = 1
        |    x = 2
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.warning(
              "Unused variable x.",
              Some(mainFile),
              Some(model.Range(model.Position(1, 4), model.Position(1, 5)))
            ),
            Api.ExecutionResult.Diagnostic.error(
              "Variable x is being redefined.",
              Some(mainFile),
              Some(model.Range(model.Position(2, 4), model.Position(2, 9)))
            )
          )
        )
      )
    )
  }

  it should "return compiler error unrecognized token" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = Panic.catch_primitive @ .convert_to_dataflow_error
        |    IO.println x
        |    IO.println (x.catch .to_text)
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
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Unrecognized token.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 30), model.Position(3, 31)))
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List(
      "(Error: (Syntax_Error 'Unrecognized token.'))",
      "(Syntax_Error 'Unrecognized token.')"
    )
  }

  it should "return compiler error syntax error" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """import Standard.Base.IO
        |from Standard.Base.Error.Common import all
        |
        |main =
        |    x = Panic.catch_primitive () .convert_to_dataflow_error
        |    IO.println (x.catch .to_text)
        |
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
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Parentheses can't be empty.",
              Some(mainFile),
              Some(model.Range(model.Position(4, 30), model.Position(4, 32)))
            )
          )
        )
      )
    )
  }

  it should "return compiler error method overloads are not supported" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """import Standard.Base.IO
        |
        |foo = 1
        |foo = 2
        |
        |main = IO.println this.foo
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
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Method overloads are not supported: here.foo is defined multiple times in this module.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 0), model.Position(3, 7)))
            )
          )
        )
      )
    )
  }

  it should "skip side effects when evaluating cached expression" in {
    val contents  = context.Main2.code
    val mainFile  = context.writeMain(contents)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

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
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main2.Update.mainY(contextId),
      context.Main2.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm expensive!", "I'm more expensive!")

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()
  }

  it should "rename a project" in {
    val contents  = context.Main.code
    val mainFile  = context.writeMain(contents)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(
        Api.OpenFileNotification(mainFile, contents)
      )
    )
    context.receiveNone shouldEqual None

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(
              "Enso_Test.Test.Main",
              "Enso_Test.Test.Main",
              "main"
            ),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // rename Test -> Foo
    context.pkg.rename("Foo")
    context.send(
      Api.Request(requestId, Api.RenameProject("Enso_Test", "Test", "Foo"))
    )
    context.receiveN(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.ProjectRenamed("Enso_Test", "Foo"))
    )

    // recompute existing stack
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    // recompute invalidating all
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(Api.InvalidatedExpressions.All())
        )
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        context.Main.idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Foo.Main", ConstantsGen.NUMBER, "foo")
      ),
      context.executionComplete(contextId)
    )
  }

  it should "send the type graph" in {
    val requestId                = UUID.randomUUID()
    val expectedGraph: TypeGraph = Types.getTypeHierarchy

    context.send(Api.Request(requestId, Api.GetTypeGraphRequest()))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.GetTypeGraphResponse(expectedGraph))
    )
  }
}
