package org.enso.interpreter.test.instrument

import java.io.{ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.UUID

import org.enso.interpreter.test.Metadata
import org.enso.pkg.Package
import org.enso.polyglot.runtime.Runtime.{Api, ApiRequest}
import org.enso.polyglot.{
  LanguageInfo,
  PolyglotContext,
  RuntimeOptions,
  RuntimeServerInfo
}
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
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

    val tmpDir: File = Files.createTempDirectory("enso-test-packages").toFile

    val pkg: Package               = Package.create(tmpDir, packageName)
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PACKAGES_PATH, pkg.root.getAbsolutePath)
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .out(out)
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

    def writeMain(contents: String): File =
      Files.write(pkg.mainFile.toPath, contents.getBytes).toFile

    def writeFile(file: File, contents: String): Unit = {
      Files.write(file.toPath, contents.getBytes): Unit
    }

    def send(msg: Api.Request): Unit = endPoint.sendBinary(Api.serialize(msg))

    def receive: Option[Api.Response] = {
      val msg = messageQueue.headOption
      messageQueue = messageQueue.drop(1)
      msg
    }

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

    object Main {

      val metadata = new Metadata

      val idMainX = metadata.addItem(16, 1)
      val idMainY = metadata.addItem(26, 7)
      val idMainZ = metadata.addItem(42, 5)
      val idFooY  = metadata.addItem(81, 8)
      val idFooZ  = metadata.addItem(98, 5)

      val code = metadata.appendToCode(
        """
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
          |""".stripMargin
      )

      object update {

        def idMainX(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainX,
                  Some("Number"),
                  Some("6"),
                  None
                )
              )
            )
          )

        def idMainY(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainY,
                  Some("Number"),
                  Some("45"),
                  Some(Api.MethodPointer(pkg.mainFile, "Number", "foo"))
                )
              )
            )
          )

        def idMainZ(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainZ,
                  Some("Number"),
                  Some("50"),
                  None
                )
              )
            )
          )

        def idFooY(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idFooY,
                  Some("Number"),
                  Some("9"),
                  None
                )
              )
            )
          )

        def idFooZ(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idFooZ,
                  Some("Number"),
                  Some("45"),
                  None
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
    val mainFile  = context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push local item on top of the empty stack
    val invalidLocalItem = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api
        .Request(requestId, Api.PushContextRequest(contextId, invalidLocalItem))
    )
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.InvalidStackItemError(contextId))),
      None
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(mainFile, "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    Set.fill(5)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PushContextResponse(contextId))),
      Some(context.Main.update.idMainX(contextId)),
      Some(context.Main.update.idMainY(contextId)),
      Some(context.Main.update.idMainZ(contextId)),
      None
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    Set.fill(4)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PushContextResponse(contextId))),
      Some(context.Main.update.idFooY(contextId)),
      Some(context.Main.update.idFooZ(contextId)),
      None
    )

    // push method pointer on top of the non-empty stack
    val invalidExplicitCall = Api.StackItem.ExplicitCall(
      Api.MethodPointer(mainFile, "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, invalidExplicitCall)
      )
    )
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.InvalidStackItemError(contextId))),
      None
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    Set.fill(5)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PopContextResponse(contextId))),
      Some(context.Main.update.idMainX(contextId)),
      Some(context.Main.update.idMainY(contextId)),
      Some(context.Main.update.idMainZ(contextId)),
      None
    )

    // pop main
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PopContextResponse(contextId))),
      None
    )

    // pop empty stack
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.EmptyStackError(contextId))),
      None
    )
  }

  it should "support file modification operations" in {
    def send(msg: ApiRequest): Unit =
      context.send(Api.Request(UUID.randomUUID(), msg))

    val fooFile   = new File(context.pkg.sourceDir, "Foo.enso")
    val contextId = UUID.randomUUID()

    send(Api.CreateContextRequest(contextId))
    context.receive

    // Create a new file
    context.writeFile(fooFile, "main = IO.println \"I'm a file!\"")

    // Open the new file
    send(
      Api.OpenFileNotification(
        fooFile,
        "main = IO.println \"I'm a file!\""
      )
    )
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    send(
      Api.PushContextRequest(
        contextId,
        Api.StackItem
          .ExplicitCall(
            Api.MethodPointer(fooFile, "Foo", "main"),
            None,
            Vector()
          )
      )
    )
    context.consumeOut shouldEqual List("I'm a file!")

    // Open the file with contents changed
    send(
      Api.OpenFileNotification(
        fooFile,
        "main = IO.println \"I'm an open file!\""
      )
    )
    context.consumeOut shouldEqual List()

    // Modify the file
    send(
      Api.EditFileNotification(
        fooFile,
        Seq(
          TextEdit(
            model.Range(model.Position(0, 24), model.Position(0, 30)),
            " modified"
          )
        )
      )
    )
    context.consumeOut shouldEqual List("I'm a modified file!")

    // Close the file
    send(Api.CloseFileNotification(fooFile))
    context.consumeOut shouldEqual List()
  }

  it should "recompute expressions" in {
    val mainFile  = context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(mainFile, "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    Set.fill(5)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PushContextResponse(contextId))),
      Some(context.Main.update.idMainX(contextId)),
      Some(context.Main.update.idMainY(contextId)),
      Some(context.Main.update.idMainZ(contextId)),
      None
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    Set.fill(5)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.RecomputeContextResponse(contextId))),
      Some(context.Main.update.idMainX(contextId)),
      Some(context.Main.update.idMainY(contextId)),
      Some(context.Main.update.idMainZ(contextId)),
      None
    )
  }

}
