package org.enso.interpreter.test.instrument

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.UUID

import org.enso.interpreter.test.Metadata
import org.enso.pkg.Package
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.{
  LanguageInfo,
  PolyglotContext,
  RuntimeOptions,
  RuntimeServerInfo
}
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RuntimeServerTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String) {
    var endPoint: MessageEndpoint        = _
    var messageQueue: List[Api.Response] = List()
    val tmpDir: File                     = Files.createTempDirectory("enso-test-packages").toFile
    val pkg: Package                     = Package.create(tmpDir, packageName)
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.getPackagesPathOption, pkg.root.getAbsolutePath)
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .serverTransport { (uri, peer) =>
          if (uri.toString == RuntimeServerInfo.URI) {
            endPoint = peer
            new MessageEndpoint {
              override def sendText(text: String): Unit = {}

              override def sendBinary(data: ByteBuffer): Unit =
                messageQueue ++= Api.deserializeResponse(data)

              override def sendPing(data: ByteBuffer): Unit = {}

              override def sendPong(data: ByteBuffer): Unit = {}

              override def sendClose(): Unit = {}
            }
          } else null
        }
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    def mkFile(name: String): File = new File(tmpDir, name)

    def writeFile(name: String, contents: String): Unit = {
      Files.write(mkFile(name).toPath, contents.getBytes): Unit
    }

    def writeMain(contents: String): Unit = {
      Files.write(pkg.mainFile.toPath, contents.getBytes): Unit
    }

    def send(msg: Api.Request): Unit = endPoint.sendBinary(Api.serialize(msg))

    def receive: Option[Api.Response] = {
      val msg = messageQueue.headOption
      messageQueue = messageQueue.drop(1)
      msg
    }
  }

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  "Runtime server" should "allow executing a stack of functions by entering them through call-sites" in {
    val metadata = new Metadata
    val idMainX  = metadata.addItem(14, 7)
    val idMainY  = metadata.addItem(30, 7)
    val idFooY   = metadata.addItem(85, 8)
    val idFooZ   = metadata.addItem(102, 5)

    context.writeMain(
      metadata.appendToCode(
        """
          |main =
          |    x = 1 + 5
          |    y = x.foo 5
          |    z = y + 5
          |    z
          |
          |Number.foo = x ->
          |    y = this + 3
          |    z = y * x
          |    z
          |""".stripMargin
      )
    )
    context.send(
      Api.Request(
        UUID.randomUUID(),
        Api.Execute(
          "Test.Main",
          "Main",
          "main",
          List(idMainY)
        )
      )
    )
    val updates = Set(context.receive, context.receive)
    updates shouldEqual Set(
      Some(Api.Response(Api.ExpressionValueUpdateNotification(idFooY, "9"))),
      Some(Api.Response(Api.ExpressionValueUpdateNotification(idFooZ, "45")))
    )
  }
}
