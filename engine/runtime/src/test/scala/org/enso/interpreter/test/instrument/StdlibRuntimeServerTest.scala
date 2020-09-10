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
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@scala.annotation.nowarn("msg=multiarg infix syntax")
class StdlibRuntimeServerTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String) {

    var endPoint: MessageEndpoint = _
    val messageQueue: LinkedBlockingQueue[Api.Response] =
      new LinkedBlockingQueue()

    val tmpDir: File = Files.createTempDirectory("enso-test-packages").toFile
    val stdlib: File =
      Paths.get("../../distribution/std-lib/Base").toFile.getAbsoluteFile

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
        .option(RuntimeOptions.LOG_LEVEL, "FINEST")
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
      paths.mkString(":")

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

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

    def executionSuccessful(contextId: UUID): Api.Response =
      Api.Response(Api.ExecutionSuccessful(contextId))

  }

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  val StdlibModules = Seq(
    "Base.Bench_Utils",
    "Base.Boolean",
    "Base.List",
    "Base.Main",
    "Base.Maybe",
    "Base.Process.Exit_Code",
    "Base.Process",
    "Base.System.Platform",
    "Base.Test",
    "Base.Vector",
    "Test.Main"
  )

  it should "re-index" in {
    val requestId = UUID.randomUUID()

    context.send(Api.Request(requestId, Api.CreateModulesIndexRequest()))
    context.receive(10) match {
      case Some(
            Api.Response(Some(`requestId`), Api.CreateModulesIndexResponse(xs))
          ) =>
        xs.map(_.moduleName) should contain theSameElementsAs StdlibModules
      case other => fail(s"Unexpected response $other")
    }

    context.send(Api.Request(requestId, Api.CreateModulesIndexRequest()))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateModulesIndexResponse(Seq()))
    )
  }

  it should "import Base" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata

    val code =
      """from Base import all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    @scala.annotation.unused
    val mainFile = context.writeMain(contents)

    context.send(Api.Request(requestId, Api.CreateModulesIndexRequest()))
    context.receive(10) match {
      case Some(
            Api.Response(Some(`requestId`), Api.CreateModulesIndexResponse(xs))
          ) =>
        xs.map(_.moduleName) should contain theSameElementsAs StdlibModules
      case other => fail(s"Unexpected response $other")
    }

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents, false))
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
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "main",
                List(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    context.consumeOut shouldEqual List("Hello World!")
  }

}
