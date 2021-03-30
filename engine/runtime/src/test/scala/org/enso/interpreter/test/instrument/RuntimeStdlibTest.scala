package org.enso.interpreter.test.instrument

import java.io.{ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.OsSpec
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeStdlibTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with OsSpec {

  final val ContextPathSeparator: String =
    if (isWindows) ";" else ":"

  var context: TestContext = _

  class TestContext(packageName: String) {

    var endPoint: MessageEndpoint = _
    val messageQueue: LinkedBlockingQueue[Api.Response] =
      new LinkedBlockingQueue()

    val tmpDir: File = Files.createTempDirectory("enso-test-packages").toFile
    val stdlib: File =
      Paths.get("../../distribution/std-lib/Standard").toFile.getAbsoluteFile

    val pkg: Package[File] =
      PackageManager.Default.create(tmpDir, packageName, "0.0.1")
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(
          RuntimeOptions.PACKAGES_PATH,
          toPackagesPath(pkg.root.getAbsolutePath, stdlib.toString)
        )
        .option(RuntimeOptions.LOG_LEVEL, "WARNING")
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .out(out)
        .serverTransport { (uri, peer) =>
          if (uri.toString == RuntimeServerInfo.URI) {
            endPoint = peer
            new MessageEndpoint {
              override def sendText(text: String): Unit = {}

              override def sendBinary(data: ByteBuffer): Unit =
                Api.deserializeResponse(data).foreach(messageQueue.add)

              override def sendPing(data: ByteBuffer): Unit = {}

              override def sendPong(data: ByteBuffer): Unit = {}

              override def sendClose(): Unit = {}
            }
          } else null
        }
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    def toPackagesPath(paths: String*): String =
      paths.mkString(ContextPathSeparator)

    def writeMain(contents: String): File =
      Files.write(pkg.mainFile.toPath, contents.getBytes).toFile

    def writeFile(file: File, contents: String): File =
      Files.write(file.toPath, contents.getBytes).toFile

    def writeInSrcDir(moduleName: String, contents: String): File = {
      val file = new File(pkg.sourceDir, s"$moduleName.enso")
      Files.write(file.toPath, contents.getBytes).toFile
    }

    def send(msg: Api.Request): Unit = endPoint.sendBinary(Api.serialize(msg))

    def receiveNone: Option[Api.Response] = {
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

  it should "import Base modules" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata

    val code =
      """from Standard.Base import all
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
      Api.Request(Api.OpenFileNotification(mainFile, contents, true))
    )
    context.receiveNone shouldEqual None

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
    val response =
      context.receiveAllUntil(
        context.executionComplete(contextId),
        timeout = 60
      )
    response should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    val suggestions = response.collect {
      case Api.Response(
            None,
            Api.SuggestionsDatabaseModuleUpdateNotification(_, _, as, xs)
          ) =>
        (xs.nonEmpty || as.nonEmpty) shouldBe true
    }
    suggestions.isEmpty shouldBe false

    val builtinsSuggestions = response.collect {
      case Api.Response(
            None,
            Api.SuggestionsDatabaseModuleUpdateNotification(file, _, as, xs)
          ) if file.getPath.contains("Builtins") =>
        (xs.nonEmpty || as.nonEmpty) shouldBe true
    }
    builtinsSuggestions.length shouldBe 1

    context.consumeOut shouldEqual List("Hello World!")
  }

}
