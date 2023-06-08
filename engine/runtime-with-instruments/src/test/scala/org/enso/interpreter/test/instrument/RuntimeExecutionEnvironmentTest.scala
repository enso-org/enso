package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.OsSpec
import org.graalvm.polyglot.Context
import org.scalatest.concurrent.{TimeLimitedTests, TimeLimits}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

import scala.concurrent.duration._

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeExecutionEnvironmentTest
    extends AnyFlatSpec
    with TimeLimitedTests
    with TimeLimits
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with OsSpec {

  import RuntimeExecutionEnvironmentTest.IF_ENABLED_METH_CALL

  override val timeLimit = 5.minutes

  final val ContextPathSeparator: String = File.pathSeparator

  var context: TestContext = _

  class TestContext(packageName: String) extends InstrumentTestContext {

    val tmpDir: Path = Files.createTempDirectory("enso-test-packages")
    sys.addShutdownHook(FileSystem.removeDirectoryIfExists(tmpDir))
    val distributionHome: File =
      Paths.get("../../distribution/component").toFile.getAbsoluteFile
    val editionHome: File =
      Paths.get("../../distribution/lib").toRealPath().toFile.getAbsoluteFile
    val edition     = TestEdition.readStdlib(editionHome)
    val lockManager = new ThreadSafeFileLockManager(tmpDir.resolve("locks"))
    val runtimeServerEmulator =
      new RuntimeServerEmulator(messageQueue, lockManager)

    val pkg: Package[File] =
      PackageManager.Default.create(
        tmpDir.toFile,
        packageName,
        "Enso_Test",
        edition = Some(edition)
      )
    val out: ByteArrayOutputStream    = new ByteArrayOutputStream()
    val logOut: ByteArrayOutputStream = new ByteArrayOutputStream()
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(
          RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
          distributionHome.toString
        )
        .option(RuntimeOptions.LOG_LEVEL, "WARNING")
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_EXECUTION_TIMER, "false")
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .option(RuntimeOptions.INTERACTIVE_MODE, "true")
        .option(
          RuntimeOptions.DISABLE_IR_CACHES,
          InstrumentTestContext.DISABLE_IR_CACHE
        )
        //.logHandler(logOut)
        .out(out)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[EnsoContext]

    def toPackagesPath(paths: String*): String =
      paths.mkString(File.pathSeparator)

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

    def analyzeJobFinished: Api.Response =
      Api.Response(Api.AnalyzeModuleInScopeJobFinished())

  }

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  override protected def afterEach(): Unit = {
    context.executionContext.context.close()
    context.runtimeServerEmulator.terminate()
  }

  it should "panic when output context is not enabled" in {

    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idRes    = metadata.addItem(97, 45)
    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Output
        |
        |main =
        |    res = Output.if_enabled (IO.println "Hello World!")
        |    res
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
    val responses1 = context.receiveNIgnoreStdLib(4)
    responses1 should contain allOf (
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        idRes,
        IF_ENABLED_METH_CALL,
        Api.ExpressionUpdate.Payload
          .Panic("Forbidden operation: Output.", Seq(idRes)),
        false
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()
    context.languageContext.getExecutionEnvironment.getName shouldEqual Api.ExecutionEnvironment
      .Design()
      .name

    // set execution environment
    context.send(
      Api.Request(
        requestId,
        Api.SetExecutionEnvironmentRequest(
          contextId,
          Api.ExecutionEnvironment.Live()
        )
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SetExecutionEnvironmentResponse(contextId)),
      Api.Response(
        None,
        Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              idRes,
              Some(ConstantsGen.NOTHING),
              Some(IF_ENABLED_METH_CALL),
              Vector(Api.ProfilingInfo.ExecutionTime(0)),
              false,
              true,
              Api.ExpressionUpdate.Payload.Value()
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")
    context.languageContext.getExecutionEnvironment.getName shouldEqual Api.ExecutionEnvironment
      .Live()
      .name
  }

  it should "panic when input is not enabled" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val idRes    = metadata.addItem(96, 19)
    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Input
        |
        |main =
        |    res = Input.if_enabled 42
        |    res
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
    val responses1 = context.receiveNIgnoreStdLib(4)
    responses1 should contain allOf (
      Api.Response(Api.BackgroundJobsStartedNotification()),
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        idRes,
        IF_ENABLED_METH_CALL,
        Api.ExpressionUpdate.Payload
          .Panic("Forbidden operation: Input.", Seq(idRes)),
        false
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()
    context.languageContext.getExecutionEnvironment.getName shouldEqual Api.ExecutionEnvironment
      .Design()
      .name

    // set execution environment
    context.send(
      Api.Request(
        requestId,
        Api.SetExecutionEnvironmentRequest(
          contextId,
          Api.ExecutionEnvironment.Live()
        )
      )
    )
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SetExecutionEnvironmentResponse(contextId)),
      Api.Response(
        None,
        Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              idRes,
              Some(ConstantsGen.INTEGER),
              Some(IF_ENABLED_METH_CALL),
              Vector(Api.ProfilingInfo.ExecutionTime(0)),
              false,
              true,
              Api.ExpressionUpdate.Payload.Value()
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.languageContext.getExecutionEnvironment.getName shouldEqual Api.ExecutionEnvironment
      .Live()
      .name
  }

}

object RuntimeExecutionEnvironmentTest {

  private val IF_ENABLED_METH_CALL =
    Api.MethodCall(
      Api.MethodPointer(
        "Standard.Base.Runtime",
        "Standard.Base.Runtime.Context",
        "if_enabled"
      ),
      Vector(2)
    )
}
