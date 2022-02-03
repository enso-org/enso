package org.enso.interpreter.test.instrument

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.editions.LibraryName
import org.enso.interpreter.runtime
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{
  Component,
  ComponentGroups,
  ExtendedComponentGroup,
  ModuleName,
  ModuleReference,
  Package,
  PackageManager
}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.OsSpec
import org.graalvm.polyglot.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeComponentsTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with OsSpec {

  final val ContextPathSeparator: String = File.pathSeparator

  var context: TestContext = _

  class TestContext(packageName: String) {

    val messageQueue: LinkedBlockingQueue[Api.Response] =
      new LinkedBlockingQueue()

    val tmpDir: Path = Files.createTempDirectory("enso-test-packages")
    sys.addShutdownHook(FileSystem.removeDirectoryIfExists(tmpDir))
    val distributionHome: File =
      Paths.get("../../distribution/component").toFile.getAbsoluteFile
    val lockManager = new ThreadSafeFileLockManager(tmpDir.resolve("locks"))
    val runtimeServerEmulator =
      new RuntimeServerEmulator(messageQueue, lockManager)

    val pkg: Package[File] = {
      val componentGroups =
        ComponentGroups(
          newGroups = List(),
          extendedGroups = List(
            ExtendedComponentGroup(
              module = ModuleReference(
                LibraryName("Standard", "Base"),
                ModuleName("Group2")
              ),
              color   = None,
              icon    = None,
              exports = List(Component("foo", None))
            )
          )
        )
      PackageManager.Default.create(
        root            = tmpDir.toFile,
        name            = packageName,
        namespace       = "Enso_Test",
        componentGroups = componentGroups
      )
    }

    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
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
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .option(RuntimeOptions.INTERACTIVE_MODE, "true")
        .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
        .out(out)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[runtime.Context]

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

    def receiveOne: Option[Api.Response] = {
      Option(messageQueue.poll())
    }

    def receive: Option[Api.Response] = {
      Option(messageQueue.poll(3, TimeUnit.SECONDS))
    }

    def receive(timeout: Long): Option[Api.Response] = {
      Option(messageQueue.poll(timeout, TimeUnit.SECONDS))
    }

    def receiveN(n: Int): List[Api.Response] = {
      Iterator.continually(receive).take(n).flatten.toList
    }

    def receiveN(n: Int, timeout: Long): List[Api.Response] = {
      Iterator.continually(receive(timeout)).take(n).flatten.toList
    }

    def receiveAllUntil(
      msg: Api.Response,
      timeout: Long
    ): List[Api.Response] = {
      val receivedUntil = Iterator
        .continually(receive(timeout))
        .takeWhile(received => received.isDefined && !received.contains(msg))
        .flatten
        .toList
      receivedUntil :+ msg
    }

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
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  it should "load library extended by the component group" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

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

    // open file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveOne shouldEqual None

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    val responses =
      context.receiveAllUntil(
        context.executionComplete(contextId),
        timeout = 60
      )
    // sanity check
    responses should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )

    // check LibraryLoaded notifications
    val contentRootNotifications = responses.collect {
      case Api.Response(
            None,
            Api.LibraryLoaded(namespace, name, version, _)
          ) =>
        (namespace, name, version)
    }

    val libraryVersion = buildinfo.Info.stdLibVersion
    contentRootNotifications should contain(
      ("Standard", "Base", libraryVersion)
    )

    // check the registered component groups
    val components = context.languageContext.getPackageRepository.getComponents
    val expectedComponents = Map(
      LibraryName("Enso_Test", "Test") ->
      context.pkg.config.componentGroups
        .getOrElse(fail("Unexpected config value.")),
      LibraryName("Standard", "Base")     -> ComponentGroups.empty,
      LibraryName("Standard", "Builtins") -> ComponentGroups.empty
    )
    components should contain theSameElementsAs expectedComponents

    context.consumeOut shouldEqual List()
  }

}
