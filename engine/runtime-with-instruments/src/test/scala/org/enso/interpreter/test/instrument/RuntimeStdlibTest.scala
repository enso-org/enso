package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager, QualifiedName}
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
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.logging.Level
import scala.collection.mutable
import scala.concurrent.duration._

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeStdlibTest
    extends AnyFlatSpec
    with TimeLimitedTests
    with TimeLimits
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with OsSpec {

  import RuntimeStdlibTest._

  override val timeLimit = 5.minutes

  final val ContextPathSeparator: String = File.pathSeparator

  var context: TestContext = _

  class TestContext(packageName: String) {

    val messageQueue: LinkedBlockingQueue[Api.Response] =
      new LinkedBlockingQueue()

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
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName)
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .option(RuntimeOptions.INTERACTIVE_MODE, "true")
        .option(
          RuntimeOptions.DISABLE_IR_CACHES,
          InstrumentTestContext.DISABLE_IR_CACHE
        )
        .out(out)
        .logHandler(System.err)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

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
      msgs: Seq[Api.Response],
      timeout: Long
    ): List[Api.Response] = {
      val toSee                          = collection.mutable.ArrayBuffer.from(msgs)
      var lastSeen: Option[Api.Response] = None
      val receivedUntil = Iterator
        .continually(receive(timeout))
        .takeWhile {
          case Some(received) =>
            val found = msgs.contains(received)
            if (found) {
              lastSeen = Some(received)
              toSee.subtractOne(received)
            }
            !(found && toSee.isEmpty)
          case None =>
            false
        }
        .flatten
        .toList
      receivedUntil :++ lastSeen
    }

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

  def extractTypes(suggestion: Suggestion): Seq[QualifiedName] = {
    val arguments  = Suggestion.Arguments(suggestion)
    val argTypes   = arguments.map(_.reprType)
    val selfType   = Suggestion.SelfType(suggestion)
    val returnType = suggestion.returnType

    (argTypes ++ selfType :+ returnType).map(QualifiedName.fromString)
  }

  /** Checks if a given type name has been resolved.
    *
    * If a type in the type signature has not been resolved, its name will
    * consist only of one segment. Every resolved type name will be mapped to its
    * qualified type name which must have at least 3 parts (as the project name
    * itself is two parts minimum).
    */
  def isResolved(name: QualifiedName): Boolean =
    name.path.size > 1

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  override protected def afterEach(): Unit = {
    context.executionContext.context.close()
    context.runtimeServerEmulator.terminate()
  }

  it should "import Base modules" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata

    val imports = context.edition.libraries.keys
      .filter(_.name != "Base")
      .map(lib => s"import ${lib.namespace}.${lib.name}")
      .mkString(System.lineSeparator())

    val code =
      s"""from Standard.Base import all
         |
         |$imports
         |
         |main = IO.println "Hello World!"
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
        Seq(context.executionComplete(contextId)),
        timeout = 180
      )
    // sanity check
    responses should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId),
    )

    // check that the suggestion notifications are received
    val errors: mutable.Map[String, Vector[ErrorEntry]] =
      mutable.Map()
    val suggestions = responses.collect {
      case Api.Response(
            None,
            Api.SuggestionsDatabaseModuleUpdateNotification(
              module,
              actions,
              _,
              updates
            )
          ) =>
        (actions.nonEmpty || updates.nonEmpty) shouldBe true
        updates.toVector.foreach { update =>
          val types           = extractTypes(update.suggestion).toSet
          val unresolvedTypes = types.filterNot(isResolved)
          if (unresolvedTypes.nonEmpty) {
            errors.updateWith(module) {
              case Some(values) =>
                Some(values :+ ErrorEntry(update.suggestion, unresolvedTypes))
              case None =>
                Some(Vector(ErrorEntry(update.suggestion, unresolvedTypes)))
            }
          }
        }
    }
    suggestions.isEmpty shouldBe false

    // check that types are qualified
    if (errors.nonEmpty) {
      val numberOfErrors = errors.size
      val report =
        errors
          .map { case (module, entries) =>
            val suggestions = entries.map(entry =>
              s"${entry.suggestion.name}(${entry.unresolvedTypes.mkString(",")})"
            )
            s"$module: ${suggestions.mkString(",")}"
          }
          .mkString("\n")
      fail(
        s"Found $numberOfErrors modules with unresolved types in method signatures:\n$report"
      )
    }

    // check LibraryLoaded notifications
    val contentRootNotifications = responses.collect {
      case Api.Response(
            None,
            Api.LibraryLoaded(namespace, name, version, _)
          ) =>
        (namespace, name, version)
    }

    val expectedLibraries = context.edition.libraries.keys.map { lib =>
      (lib.namespace, lib.name, TestEdition.testLibraryVersion.toString)
    }
    contentRootNotifications should contain theSameElementsAs expectedLibraries

    context.consumeOut shouldEqual List("Hello World!")
  }

}

object RuntimeStdlibTest {

  case class ErrorEntry(
    suggestion: Suggestion,
    unresolvedTypes: Set[QualifiedName] = Set()
  )
}
