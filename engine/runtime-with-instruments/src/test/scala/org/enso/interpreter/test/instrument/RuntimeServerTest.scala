package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.runtime.`type`.{Constants, ConstantsGen, Types}
import org.enso.interpreter.runtime.EnsoContext
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

      val idMainX = metadata.addItem(63, 1, "aa1")
      val idMainY = metadata.addItem(73, 7, "aa2")
      val idMainZ = metadata.addItem(89, 5, "aa3")
      val idFooY  = metadata.addItem(133, 8, "ff2")
      val idFooZ  = metadata.addItem(150, 5, "ff3")

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
            |Number.foo self = x ->
            |    y = self + 3
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

        def pendingZ(): Api.ExpressionUpdate =
          Api.ExpressionUpdate(
            Main.idFooZ,
            None,
            None,
            Vector(),
            true,
            Api.ExpressionUpdate.Payload.Pending(None, None)
          )

        def pendingY(): Api.ExpressionUpdate =
          Api.ExpressionUpdate(
            Main.idFooY,
            None,
            None,
            Vector(),
            true,
            Api.ExpressionUpdate.Payload.Pending(None, None)
          )

        def mainY(
          contextId: UUID,
          fromCache: Boolean = false
        ): Api.Response =
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
      val idMainY  = metadata.addItem(178, 5)
      val idMainZ  = metadata.addItem(192, 5)

      val code = metadata.appendToCode(
        """from Standard.Base import all
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
          |    y = foo x
          |    z = bar y
          |    z
          |""".stripMargin.linesIterator.mkString("\n")
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
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
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

  it should "push method with default arguments on top of the stack" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idFoo    = metadata.addItem(41, 6)

    val code =
      """from Standard.Base import all
        |
        |foo x=0 = x + 42
        |
        |main =
        |    IO.println foo
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
            Api.MethodPointer(moduleName, moduleName, "foo"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idFoo, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()
  }

  it should "push method with default arguments on the stack" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(54, 19)
    val idMainFoo = metadata.addItem(70, 3)

    val code =
      """from Standard.Base import all
        |
        |foo a=0 = a + 1
        |
        |main =
        |    IO.println foo
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
      TestMessages.update(
        contextId,
        idMainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
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

  it should "send method pointer updates of methods" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(105, 120)
    val idMainX  = metadata.addItem(132, 9)
    val idMainY  = metadata.addItem(150, 3)
    val idMainM  = metadata.addItem(162, 8)
    val idMainP  = metadata.addItem(179, 5)
    val idMainQ  = metadata.addItem(193, 5)
    val idMainF  = metadata.addItem(215, 9)

    val code =
      """from Standard.Base import all
        |import Enso_Test.Test.A
        |
        |type QuuxT
        |    Quux
        |
        |    foo = 42
        |
        |bar = 7
        |
        |main =
        |    f a b = a + b
        |    x = QuuxT.foo
        |    y = bar
        |    m = A.AT.A x
        |    p = m.foo
        |    q = A.bar
        |    IO.println (f x+y p+q)
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    val aCode =
      """
        |type AT
        |    A un_a
        |
        |    foo self = 11
        |
        |bar = 19
        |""".stripMargin.linesIterator.mkString("\n")
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(10) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainX,
        ConstantsGen.INTEGER,
        Api.MethodPointer(
          "Enso_Test.Test.Main",
          "Enso_Test.Test.Main.QuuxT.type",
          "foo"
        )
      ),
      TestMessages.update(
        contextId,
        idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "bar")
      ),
      TestMessages.update(
        contextId,
        idMainM,
        "Enso_Test.Test.A.AT",
        Api.MethodPointer("Enso_Test.Test.A", "Enso_Test.Test.A.AT", "A")
      ),
      TestMessages.update(
        contextId,
        idMainP,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Test.A", "Enso_Test.Test.A.AT", "foo")
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

  it should "send method pointer updates of constructors" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idA      = metadata.addItem(47, 3, "aa")
    val idB      = metadata.addItem(59, 6, "ab")
    val idC      = metadata.addItem(70, 7, "ac")

    val code =
      """type T
        |    A
        |    B x
        |    C y z
        |
        |main =
        |    a = T.A
        |    b = T.B 42
        |    T.C a b
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
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idA,
        "Enso_Test.Test.Main.T",
        Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main.T", "A")
      ),
      TestMessages.update(
        contextId,
        idB,
        "Enso_Test.Test.Main.T",
        Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main.T", "B")
      ),
      TestMessages.update(
        contextId,
        idC,
        "Enso_Test.Test.Main.T",
        Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main.T", "C")
      ),
      context.executionComplete(contextId)
    )
  }

  it should "send updates from last line" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(23, 12)
    val idMainFoo = metadata.addItem(28, 7)

    val code =
      """foo a b = a + b
        |
        |main =
        |    foo 1 2
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
    context.receiveN(5) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainFoo,
        ConstantsGen.INTEGER_BUILTIN,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )
  }

  it should "compute side effects correctly from last line" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(54, 25)
    val idMainFoo = metadata.addItem(71, 7)

    val code =
      """from Standard.Base import all
        |
        |foo a b = a + b
        |
        |main =
        |    IO.println (foo 1 2)
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
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
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
    val idMain    = metadata.addItem(73, 36)
    val idMainBar = metadata.addItem(105, 3)

    val code =
      """from Standard.Base import all
        |import Standard.Base.Runtime.State
        |
        |main = IO.println (State.run Number 42 bar)
        |
        |bar = State.get Number
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
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idMainBar,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "bar")
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
    val idMain    = metadata.addItem(73, 35)
    val idMainBar = metadata.addItem(104, 3)

    val code =
      """from Standard.Base import all
        |import Standard.Base.Runtime.State
        |
        |main = IO.println (State.run Number 0 bar)
        |
        |bar =
        |    State.put Number 10
        |    State.get Number
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
      TestMessages.update(
        contextId,
        idMainBar,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "bar")
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
    val idMain    = metadata.addItem(23, 18)
    val idMainFoo = metadata.addItem(28, 7)

    val code =
      """foo a b = a + b
        |
        |main =
        |    foo 1 2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
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
        ConstantsGen.INTEGER_BUILTIN,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER_BUILTIN),
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
    val fooX    = metadata.addItem(45, 1, "aa")
    val fooRes  = metadata.addItem(51, 1, "ab")
    val mainFoo = metadata.addItem(69, 3, "ac")
    val mainRes = metadata.addItem(77, 12, "ad")

    val code =
      """from Standard.Base import all
        |
        |foo =
        |    x = 4
        |    x
        |
        |main =
        |    y = foo
        |    IO.println y
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
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
        .update(
          contextId,
          mainFoo,
          ConstantsGen.INTEGER,
          Api.MethodPointer(moduleName, moduleName, "foo")
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
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
          ),
          execute = true
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, fooX, fooRes, mainFoo, mainRes),
      TestMessages.update(contextId, fooX, ConstantsGen.INTEGER),
      TestMessages.update(contextId, fooRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("5")

    // pop the foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
      TestMessages.update(
        contextId,
        mainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("5")
  }

  it should "obey the execute parameter of edit command" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    // foo definition
    metadata.addItem(31, 22)
    // foo name
    metadata.addItem(31, 3)
    val mainFoo = metadata.addItem(69, 3)
    val mainRes = metadata.addItem(77, 12)

    val code =
      """from Standard.Base import all
        |
        |foo =
        |    x = 4
        |    x
        |
        |main =
        |    y = foo
        |    IO.println y
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
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
      TestMessages.update(
        contextId,
        mainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
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
          ),
          execute = false
        )
      )
    )
    context.receiveNone shouldEqual None

    // Modify the foo method
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 8), model.Position(3, 9)),
              "6"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, mainFoo, mainRes),
      TestMessages.update(
        contextId,
        mainFoo,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "foo")
      ),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("6")
  }

  it should "send updates when the type is not changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val idMain     = context.Main.metadata.addItem(54, 46, "aaaaa")
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(7) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
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
    val idResult  = metadata.addItem(51, 4, "aae")
    val idPrintln = metadata.addItem(60, 17, "aaf")
    val idMain    = metadata.addItem(37, 40, "aaa")
    val code =
      """from Standard.Base import all
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, idResult, idPrintln, idMain),
      TestMessages.update(contextId, idResult, ConstantsGen.TEXT),
      TestMessages.update(contextId, idPrintln, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hi")
  }

  it should "send updates when the method pointer is changed" in {
    val contextId      = UUID.randomUUID()
    val requestId      = UUID.randomUUID()
    val moduleName     = "Enso_Test.Test.Main"
    val numberTypeName = "Standard.Base.Data.Numbers.Number"

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 34, "aaaa")
    val idMainA  = metadata.addItem(46, 8, "aabb")
    val idMainP  = metadata.addItem(59, 12, "aacc")
    // pie id
    metadata.addItem(89, 1, "eee")
    // uwu id
    metadata.addItem(87, 1, "bbb")
    // hie id
    metadata.addItem(95, 6, "fff")
    // Number.x id
    metadata.addItem(115, 1, "999")
    val code =
      """from Standard.Base import all
        |
        |main =
        |    a = 123 + 21
        |    IO.println a
        |
        |pie = 3
        |uwu = 7
        |hie = "hie!"
        |Number.x self y = y
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
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
              model.Range(model.Position(3, 8), model.Position(3, 16)),
              "1234.x 4"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, idMain, idMainA, idMainP),
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "x")
      ),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
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
              model.Range(model.Position(3, 8), model.Position(3, 16)),
              "1000.x 5"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) shouldEqual Seq(
      TestMessages.pending(contextId, idMain, idMainA, idMainP),
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, numberTypeName, "x")
      ),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("5")

    // Edit s/1000.x 5/Main.pie/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 8), model.Position(3, 16)),
              "Main.pie"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, idMain, idMainA, idMainP),
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "pie")
      ),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
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
              model.Range(model.Position(3, 8), model.Position(3, 16)),
              "Main.uwu"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, idMain, idMainA, idMainP),
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, moduleName, "uwu")
      ),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
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
              model.Range(model.Position(3, 8), model.Position(3, 16)),
              "Main.hie"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, idMain, idMainA, idMainP),
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.TEXT,
        Api.MethodPointer(moduleName, moduleName, "hie")
      ),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
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
              model.Range(model.Position(3, 8), model.Position(3, 16)),
              "\"Hello!\""
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, idMain, idMainA, idMainP),
      TestMessages.update(
        contextId,
        idMainA,
        ConstantsGen.TEXT,
        Api.MethodPointer(moduleName, moduleName, "hie")
      ),
      TestMessages.update(contextId, idMainP, ConstantsGen.NOTHING),
      TestMessages.update(contextId, idMain, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello!")
  }

  it should "send updates for overloaded functions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(122, 87, "aaaa")
    val id1      = metadata.addItem(131, 15, "aad1")
    val id2      = metadata.addItem(151, 18, "aad2")
    val id3      = metadata.addItem(174, 15, "aad3")
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
        |Text.overloaded self arg = arg + 1
        |Number.overloaded self arg = arg + 2
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
    context.receiveNIgnoreStdLib(7) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    context.receiveN(6) should contain theSameElementsAs Seq(
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
      TestMessages.update(
        contextId,
        idMain,
        ConstantsGen.NOTHING
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
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
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
      TestMessages.update(
        contextId,
        idMain,
        ConstantsGen.NOTHING
      ),
      TestMessages.update(
        contextId,
        id1,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded"),
        fromCache = true
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
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
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
      TestMessages.update(
        contextId,
        idMain,
        ConstantsGen.NOTHING
      ),
      TestMessages.update(
        contextId,
        id1,
        ConstantsGen.INTEGER,
        Api.MethodPointer(moduleName, ConstantsGen.NUMBER, "overloaded"),
        fromCache = true
      ),
      context.executionComplete(contextId)
    )
  }

  it should "send updates for a lambda" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val xId     = metadata.addItem(46, 10)
    val mainRes = metadata.addItem(61, 12)

    val code =
      """from Standard.Base import all
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

    // Set sources for the module
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
      TestMessages.update(contextId, xId, ConstantsGen.FUNCTION),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
  }

  it should "send updates for a constructor type" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val idMain = metadata.addItem(39, 27)

    val code =
      """type My_Type
        |    My_Constructor
        |
        |main =
        |    My_Type.My_Constructor
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, s"$moduleName.My_Type"),
      context.executionComplete(contextId)
    )
  }

  it should "support file modification operations without attached ids" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val code =
      """import Standard.Base.IO
        |
        |main = IO.println "I'm a file!"
        |""".stripMargin.linesIterator.mkString("\n")

    // Create a new file
    val mainFile = context.writeMain(code)

    // Set sources for the module
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
          ),
          execute = true
        )
      )
    )
    context.receiveN(1) shouldEqual Seq(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm a modified!")

    // Close the file
    context.send(Api.Request(Api.CloseFileNotification(mainFile)))
    context.consumeOut shouldEqual List()
  }

  it should "support file modifications after reopening the file" in {
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
        |""".stripMargin.linesIterator.mkString("\n")

    // Create a new file
    val mainFile = context.writeMain(code)

    // Set sources for the module
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
    context.consumeOut shouldEqual List("I'm a file!")

    // Close the file
    context.send(Api.Request(Api.CloseFileNotification(mainFile)))
    context.consumeOut shouldEqual List()

    val contextId2 = UUID.randomUUID()
    val requestId2 = UUID.randomUUID()

    context.send(Api.Request(requestId2, Api.CreateContextRequest(contextId2)))
    context.receive shouldEqual Some(
      Api.Response(requestId2, Api.CreateContextResponse(contextId2))
    )

    // Re-open the the file and apply the same operation
    context.send(Api.Request(Api.OpenFileNotification(mainFile, code)))
    context.receiveNone shouldEqual None
    context.consumeOut shouldEqual List()

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
          ),
          execute = true
        )
      )
    )
    context.receiveN(1) shouldEqual Seq(
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("I'm a modified!")

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

    // Set sources for the module
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
              Api.MethodPointer(moduleName, moduleName, "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER_BUILTIN),
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
          ),
          execute = true
        )
      )
    )
    context.receiveN(3) shouldEqual Seq(
      TestMessages.pending(contextId, idMain),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )
  }

  it should "send suggestion notifications when file is executed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val idMain     = context.Main.metadata.addItem(54, 46, "aaaa")

    val mainFile = context.writeMain(context.Main.code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, context.Main.code))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(7) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
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
        |""".stripMargin.linesIterator.mkString("\n")

    // Create a new file
    val mainFile = context.writeMain(code)

    // Set sources for the module
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
    context.consumeOut shouldEqual List("I'm a file!")

    /*
      Modify the file:
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.IO
        |
        |Number.lucky = 42
        |
        |main = IO.println "I'm a modified!"
        |""".stripMargin.linesIterator.mkString("\n")
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
          ),
          execute = true
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

  it should "send expression updates when file is restored" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val metadata = new Metadata
    val idText   = metadata.addItem(49, 12, "aa")
    val idRes    = metadata.addItem(66, 15, "ab")

    def template(text: String) =
      metadata.appendToCode(
        s"""from Standard.Base import all
           |
           |main =
           |    text = "$text"
           |    IO.println text
           |""".stripMargin.linesIterator.mkString("\n")
      )

    val prompt1 = "I'm a one!"
    val code    = template(prompt1)

    // Create a new file
    val mainFile = context.writeMain(code)

    // Set sources for the module
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
      TestMessages.update(contextId, idText, ConstantsGen.TEXT),
      TestMessages.update(contextId, idRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List(prompt1)

    // Simulate file update in FS
    val prompt2 = "I'm a two!"
    val code2   = template(prompt2)
    context.writeMain(code2)

    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 0), model.Position(9, 2)),
              code2
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      TestMessages.update(contextId, idText, ConstantsGen.TEXT),
      TestMessages.update(contextId, idRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List(prompt2)

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

    // Set sources for the module
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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

    // Set sources for the module
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.pending(
        contextId,
        context.Main.idMainX,
        context.Main.idMainY,
        context.Main.idMainZ,
        context.Main.idFooY,
        context.Main.idFooZ
      ),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
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

    // Set sources for the module
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )

    context.receiveNone shouldEqual None
    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.pending(contextId, context.Main.idMainZ),
      context.Main.Update.mainZ(contextId),
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

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          Api.ExecutionResult.Failure(
            "Type Unexpected not found in module Enso_Test.Test.Main.",
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

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
      """main = bar 40 2 123
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

    // Set sources for the module
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Type error: expected a function, but got 42 (Integer).",
              Some(mainFile),
              Some(model.Range(model.Position(0, 7), model.Position(0, 19))),
              None,
              Vector(
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(0, 7), model.Position(0, 19))
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
      """main = bar .x .y
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

    // Set sources for the module
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
                    model.Range(model.Position(0, 7), model.Position(0, 16))
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
      """main = bar "one" 2
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

    // Set sources for the module
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
                    model.Range(model.Position(0, 7), model.Position(0, 18))
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

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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

  it should "return error with a stack trace" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """main =
        |    foo
        |
        |foo =
        |    x = bar
        |    x
        |bar =
        |    x = baz
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

    // Set sources for the module
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
                    model.Range(model.Position(7, 8), model.Position(7, 11))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.foo",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(4, 8), model.Position(4, 11))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(1, 4), model.Position(1, 7))
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

    // Set sources for the module
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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

    // Set sources for the module
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
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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

    // Set sources for the module
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
        |    x = Panic.catch_primitive ` .convert_to_dataflow_error
        |    IO.println x
        |    IO.println (x.catch Any .to_text)
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Unexpected expression.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 30), model.Position(3, 31)))
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List(
      "(Error: (Syntax_Error.Error 'Unexpected expression.'))",
      "(Syntax_Error.Error 'Unexpected expression.')"
    )
  }

  it should "return compiler error syntax error" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val metadata   = new Metadata

    val code =
      """import Standard.Base.IO
        |import Standard.Base.Panic.Panic
        |import Standard.Base.Any.Any
        |
        |main =
        |    x = Panic.catch_primitive () .convert_to_dataflow_error
        |    IO.println (x.catch Any .to_text)
        |
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Parentheses can't be empty.",
              Some(mainFile),
              Some(model.Range(model.Position(5, 30), model.Position(5, 32)))
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
        |main = IO.println foo
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
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
    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            Api.ExecutionResult.Diagnostic.error(
              "Method overloads are not supported: foo is defined multiple times in this module.",
              Some(mainFile),
              Some(model.Range(model.Position(3, 0), model.Position(3, 7)))
            )
          )
        )
      )
    )
  }

  it should "skip side effects when evaluating cached expression" in {
    val contents   = context.Main2.code
    val mainFile   = context.writeMain(contents)
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    val renameProjectResponses = context.receiveN(6)
    renameProjectResponses should contain allOf (
      Api.Response(requestId, Api.ProjectRenamed("Enso_Test", "Foo")),
      context.Main.Update.mainX(contextId),
      TestMessages.update(
        contextId,
        context.Main.idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Foo.Main", ConstantsGen.NUMBER, "foo")
      ),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )
    renameProjectResponses.collect {
      case Api.Response(
            _,
            notification: Api.SuggestionsDatabaseModuleUpdateNotification
          ) =>
        notification.module shouldEqual moduleName
        notification.actions should contain theSameElementsAs Vector(
          Api.SuggestionsDatabaseAction.Clean(moduleName)
        )
    }

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
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.pending(
        contextId,
        context.Main.idMainX,
        context.Main.idMainY,
        context.Main.idMainZ,
        context.Main.idFooY,
        context.Main.idFooZ
      ),
      context.Main.Update.mainX(contextId),
      TestMessages.update(
        contextId,
        context.Main.idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Foo.Main", ConstantsGen.NUMBER, "foo")
      ),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )
  }

  it should "push and pop functions after renaming the project" in {
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

    // open file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
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
    val renameProjectResponses = context.receiveN(6)
    renameProjectResponses should contain allOf (
      Api.Response(requestId, Api.ProjectRenamed("Enso_Test", "Foo")),
      context.Main.Update.mainX(contextId),
      TestMessages.update(
        contextId,
        context.Main.idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Foo.Main", ConstantsGen.NUMBER, "foo")
      ),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )
    renameProjectResponses.collect {
      case Api.Response(
            _,
            notification: Api.SuggestionsDatabaseModuleUpdateNotification
          ) =>
        notification.module shouldEqual moduleName
        notification.actions should contain theSameElementsAs Vector(
          Api.SuggestionsDatabaseAction.Clean(moduleName)
        )
    }

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionComplete(contextId)
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(
        contextId,
        context.Main.idMainY,
        ConstantsGen.INTEGER,
        Api.MethodPointer("Enso_Test.Foo.Main", ConstantsGen.NUMBER, "foo"),
        fromCache = true
      ),
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

  it should "send the type graph" in {
    val requestId                = UUID.randomUUID()
    val expectedGraph: TypeGraph = Types.getTypeHierarchy

    context.send(Api.Request(requestId, Api.GetTypeGraphRequest()))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.GetTypeGraphResponse(expectedGraph))
    )
  }

  it should "send updates for values annotated with warning" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idX1     = metadata.addItem(47, 14)
    val idX2     = metadata.addItem(71, 32)
    val idX3     = metadata.addItem(113, 32)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    x1 = attach "x" "y"
        |    x2 = attach "x" (My_Warning.Value 42)
        |    x3 = attach "x" (My_Warning.Value x2)
        |    [x1, x2, x3]
        |
        |type My_Warning
        |    Value reason
        |
        |attach value warning =
        |    Warning.attach_with_stacktrace value warning Runtime.primitive_get_stack_trace
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
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages
        .update(
          contextId,
          idX1,
          ConstantsGen.TEXT,
          methodPointer =
            Some(Api.MethodPointer(moduleName, moduleName, "attach")),
          payload = Api.ExpressionUpdate.Payload.Value(
            Some(Api.ExpressionUpdate.Payload.Value.Warnings(1, Some("'y'")))
          )
        ),
      TestMessages
        .update(
          contextId,
          idX2,
          ConstantsGen.TEXT,
          methodPointer =
            Some(Api.MethodPointer(moduleName, moduleName, "attach")),
          payload = Api.ExpressionUpdate.Payload.Value(
            Some(
              Api.ExpressionUpdate.Payload.Value
                .Warnings(1, Some("(My_Warning.Value 42)"))
            )
          )
        ),
      TestMessages
        .update(
          contextId,
          idX3,
          ConstantsGen.TEXT,
          methodPointer =
            Some(Api.MethodPointer(moduleName, moduleName, "attach")),
          payload = Api.ExpressionUpdate.Payload
            .Value(Some(Api.ExpressionUpdate.Payload.Value.Warnings(2, None)))
        ),
      context.executionComplete(contextId)
    )
  }

  it should "send updates for expressions annotated with warning" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idX      = metadata.addItem(46, 71)
    val idY      = metadata.addItem(126, 5)
    val idRes    = metadata.addItem(136, 12)
    val code =
      """from Standard.Base import all
        |
        |main =
        |    x = Warning.attach_with_stacktrace 42 "y" Runtime.primitive_get_stack_trace
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
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages
        .update(
          contextId,
          idX,
          ConstantsGen.INTEGER,
          payload = Api.ExpressionUpdate.Payload.Value(
            Some(Api.ExpressionUpdate.Payload.Value.Warnings(1, Some("'y'")))
          )
        ),
      TestMessages
        .update(
          contextId,
          idY,
          ConstantsGen.INTEGER,
          payload = Api.ExpressionUpdate.Payload.Value(
            Some(Api.ExpressionUpdate.Payload.Value.Warnings(1, Some("'y'")))
          )
        ),
      TestMessages
        .update(
          contextId,
          idRes,
          ConstantsGen.NOTHING,
          payload = Api.ExpressionUpdate.Payload.Value(
            Some(Api.ExpressionUpdate.Payload.Value.Warnings(1, Some("'y'")))
          )
        ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("43")
  }

  it should "send updates for values in array annotated with warning" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(37, 79)
    val code =
      """from Standard.Base import all
        |
        |main =
        |    [Warning.attach_with_stacktrace "x" "y" Runtime.primitive_get_stack_trace]
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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, idMain, ConstantsGen.VECTOR),
      context.executionComplete(contextId)
    )
  }

  it should "send pending updates for expressions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata  = new Metadata
    val x         = metadata.addItem(15, 1, "aa")
    val y         = metadata.addItem(25, 5, "ab")
    val `y_inc`   = metadata.addItem(25, 3, "ac")
    val `y_x`     = metadata.addItem(29, 1, "ad")
    val res       = metadata.addItem(35, 1, "ae")
    val `inc_res` = metadata.addItem(46, 5, "af")

    val code =
      """main =
        |    x = 1
        |    y = inc x
        |    y
        |
        |inc x = x + 1
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
    context.receiveN(8) should contain theSameElementsAs Seq(
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, x, ConstantsGen.INTEGER_BUILTIN),
      TestMessages.update(contextId, `y_inc`, Constants.UNRESOLVED_SYMBOL),
      TestMessages.update(contextId, `y_x`, ConstantsGen.INTEGER_BUILTIN),
      TestMessages.update(
        contextId,
        y,
        ConstantsGen.INTEGER_BUILTIN,
        Api.MethodPointer(moduleName, moduleName, "inc")
      ),
      TestMessages.update(contextId, res, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )

    // push inc call
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(y))
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, `inc_res`, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )

    // pop inc call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(
        contextId,
        y,
        ConstantsGen.INTEGER_BUILTIN,
        Api.MethodPointer(moduleName, moduleName, "inc"),
        fromCache = true
      ),
      TestMessages.update(contextId, res, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )

    // push inc call
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(y))
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, `inc_res`, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )

    // Modify the inc method
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 12), model.Position(5, 13)),
              "2"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, `inc_res`, `y_inc`, y, res),
      TestMessages.update(contextId, `inc_res`, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )

    // pop inc call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      TestMessages.update(contextId, `y_inc`, Constants.UNRESOLVED_SYMBOL),
      TestMessages.update(contextId, `y_x`, ConstantsGen.INTEGER_BUILTIN),
      TestMessages.update(
        contextId,
        y,
        ConstantsGen.INTEGER_BUILTIN,
        Api.MethodPointer(moduleName, moduleName, "inc")
      ),
      TestMessages.update(contextId, res, ConstantsGen.INTEGER_BUILTIN),
      context.executionComplete(contextId)
    )
  }

}
