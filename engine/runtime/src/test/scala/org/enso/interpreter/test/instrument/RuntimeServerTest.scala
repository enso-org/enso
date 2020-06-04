package org.enso.interpreter.test.instrument

import java.io.{ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.UUID

import org.enso.interpreter.instrument.{
  IdExecutionInstrument,
  InstrumentFrame,
  RuntimeServerInstrument
}
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot.runtime.Runtime.Api.VisualisationUpdate
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

    val pkg: Package[File] =
      PackageManager.Default.create(tmpDir, packageName, "0.0.1")
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

    val instrument = executionContext.context.getEngine.getInstruments
      .get(RuntimeServerInfo.INSTRUMENT_NAME)
      .lookup(classOf[RuntimeServerInstrument])

    def writeMain(contents: String): File =
      Files.write(pkg.mainFile.toPath, contents.getBytes).toFile

    def writeFile(file: File, contents: String): File =
      Files.write(file.toPath, contents.getBytes).toFile

    def writeInSrcDir(moduleName: String, contents: String): File = {
      val file = new File(pkg.sourceDir, s"$moduleName.enso")
      Files.write(file.toPath, contents.getBytes).toFile
    }

    def send(msg: Api.Request): Unit = endPoint.sendBinary(Api.serialize(msg))

    def receive: Option[Api.Response] = {
      val msg = messageQueue.headOption
      messageQueue = messageQueue.drop(1)
      msg
    }

    def receive(n: Int): List[Api.Response] = {
      val (messages, tail) = messageQueue.splitAt(n)
      messageQueue = tail
      messages
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

      object Update {

        def mainX(contextId: UUID, value: String = "6") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainX,
                  Some("Number"),
                  Some(value),
                  None
                )
              )
            )
          )

        def mainY(contextId: UUID, value: String = "45") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainY,
                  Some("Number"),
                  Some(value),
                  Some(Api.MethodPointer(pkg.mainFile, "Number", "foo"))
                )
              )
            )
          )

        def mainZ(contextId: UUID, value: String = "50") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainZ,
                  Some("Number"),
                  Some(value),
                  None
                )
              )
            )
          )

        def fooY(contextId: UUID, value: String = "9") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idFooY,
                  Some("Number"),
                  Some(value),
                  None
                )
              )
            )
          )

        def fooZ(contextId: UUID, value: String = "45") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idFooZ,
                  Some("Number"),
                  Some(value),
                  None
                )
              )
            )
          )
      }
    }

    object Main2 {

      val metadata = new Metadata
      val idMainY  = metadata.addItem(148, 10)
      val idMainZ  = metadata.addItem(167, 10)

      val code = metadata.appendToCode(
        """
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

        def mainY(contextId: UUID, value: String = "15") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  idMainY,
                  Some("Number"),
                  Some(value),
                  Some(Api.MethodPointer(pkg.mainFile, "Main", "foo"))
                )
              )
            )
          )

        def mainZ(contextId: UUID, value: String = "75") =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  idMainZ,
                  Some("Number"),
                  Some(value),
                  Some(Api.MethodPointer(pkg.mainFile, "Main", "bar"))
                )
              )
            )
          )

      }
    }

    object MainWithError {

      val metadata = new Metadata

      val idMain = metadata.addItem(8, 6)

      val code = metadata.appendToCode(
        """
          |main = 1 + 2L
          |""".stripMargin
      )
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
      Some(context.Main.Update.mainX(contextId)),
      Some(context.Main.Update.mainY(contextId)),
      Some(context.Main.Update.mainZ(contextId)),
      None
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    Set.fill(4)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PushContextResponse(contextId))),
      Some(context.Main.Update.fooY(contextId)),
      Some(context.Main.Update.fooZ(contextId)),
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

  it should "recompute expressions without invalidation" in {
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
      Some(context.Main.Update.mainX(contextId)),
      Some(context.Main.Update.mainY(contextId)),
      Some(context.Main.Update.mainZ(contextId)),
      None
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.RecomputeContextResponse(contextId))),
      None
    )
  }

  it should "recompute expressions invalidating all" in {
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
      Some(context.Main.Update.mainX(contextId)),
      Some(context.Main.Update.mainY(contextId)),
      Some(context.Main.Update.mainZ(contextId)),
      None
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
    Set.fill(5)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.RecomputeContextResponse(contextId))),
      Some(context.Main.Update.mainX(contextId)),
      Some(context.Main.Update.mainY(contextId)),
      Some(context.Main.Update.mainZ(contextId)),
      None
    )
  }

  it should "recompute expressions invalidating some" in {
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
      Some(context.Main.Update.mainX(contextId)),
      Some(context.Main.Update.mainY(contextId)),
      Some(context.Main.Update.mainZ(contextId)),
      None
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
    Set.fill(3)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.RecomputeContextResponse(contextId))),
      Some(context.Main.Update.mainZ(contextId)),
      None
    )
  }

  it should "return error when computing erroneous code" in {
    val mainFile  = context.writeMain(context.MainWithError.code)
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
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(
        Api.Response(
          requestId,
          Api.ExecutionFailed(contextId, "error in function: main")
        )
      ),
      None
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    Set.fill(2)(context.receive) shouldEqual Set(
      Some(
        Api.Response(
          requestId,
          Api.ExecutionFailed(contextId, "error in function: main")
        )
      ),
      None
    )
  }

  it should "skip side effects when evaluating cached expression" in {
    val file      = context.writeMain(context.Main2.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(file, "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    Set.fill(4)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.PushContextResponse(contextId))),
      Some(context.Main2.Update.mainY(contextId)),
      Some(context.Main2.Update.mainZ(contextId)),
      None
    )

    context.consumeOut shouldEqual List("I'm expensive!", "I'm more expensive!")

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )

    Set.fill(2)(context.receive) shouldEqual Set(
      Some(Api.Response(requestId, Api.RecomputeContextResponse(contextId))),
      None
    )
    context.consumeOut shouldEqual List()
  }

  it should "emit visualisation update when expression is evaluated" in {
    val mainFile = context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    send(
      Api.OpenFileNotification(visualisationFile, context.Visualisation.code)
    )

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

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

    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId)
    )

    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          context.Main.idMainX,
          Api.VisualisationConfiguration(
            contextId,
            "Test.Visualisation",
            "x -> here.encode x"
          )
        )
      )
    )
    val attachVisualisationResponses = context.receive(2)
    attachVisualisationResponses should contain(
      Api.Response(requestId, Api.VisualisationAttached())
    )
    val expectedExpressionId = context.Main.idMainX
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
          None,
          Api.VisualisationUpdate(
            Api.VisualisationContext(
              `visualisationId`,
              `contextId`,
              `expectedExpressionId`
            ),
            data
          )
          ) =>
        data
    }
    data.sameElements("6".getBytes) shouldBe true

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receive should contain(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId))
    )

    // recompute invalidating x
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(
            Api.InvalidatedExpressions.Expressions(Vector(context.Main.idMainX))
          )
        )
      )
    )
    val recomputeResponses2 = context.receive(3)
    recomputeResponses2 should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
    )
    val Some(data2) = recomputeResponses2.collectFirst {
      case Api.Response(
          None,
          Api.VisualisationUpdate(
            Api.VisualisationContext(
              `visualisationId`,
              `contextId`,
              `expectedExpressionId`
            ),
            data
          )
          ) =>
        data
    }
    data2.sameElements("6".getBytes) shouldBe true
  }

  it should "be able to modify visualisations" in {
    val mainFile = context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    send(
      Api.OpenFileNotification(visualisationFile, context.Visualisation.code)
    )

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

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

    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId)
    )

    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          context.Main.idMainX,
          Api.VisualisationConfiguration(
            contextId,
            "Test.Visualisation",
            "x -> here.encode x"
          )
        )
      )
    )

    val attachVisualisationResponses = context.receive(2)
    attachVisualisationResponses should contain(
      Api.Response(requestId, Api.VisualisationAttached())
    )
    val expectedExpressionId = context.Main.idMainX
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
          None,
          Api.VisualisationUpdate(
            Api.VisualisationContext(
              `visualisationId`,
              `contextId`,
              `expectedExpressionId`
            ),
            data
          )
          ) =>
        data
    }
    data.sameElements("6".getBytes) shouldBe true

    context.send(
      Api.Request(
        requestId,
        Api.ModifyVisualisation(
          visualisationId,
          Api.VisualisationConfiguration(
            contextId,
            "Test.Visualisation",
            "x -> here.incAndEncode x"
          )
        )
      )
    )
    val modifyVisualisationResponses = context.receive(2)
    modifyVisualisationResponses should contain(
      Api.Response(requestId, Api.VisualisationModified())
    )
    val Some(dataAfterModification) =
      modifyVisualisationResponses.collectFirst {
        case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `expectedExpressionId`
              ),
              data
            )
            ) =>
          data
      }
    dataAfterModification.sameElements("7".getBytes) shouldBe true
  }

  it should "not emit visualisation updates when visualisation is detached" in {
    val mainFile = context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    send(
      Api.OpenFileNotification(visualisationFile, context.Visualisation.code)
    )

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          context.Main.idMainX,
          Api.VisualisationConfiguration(
            contextId,
            "Test.Visualisation",
            "x -> here.encode x"
          )
        )
      )
    )
    context.receive shouldBe Some(
      Api.Response(requestId, Api.VisualisationAttached())
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

    context.receive(5) should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
    )

    context.send(
      Api.Request(
        requestId,
        Api.DetachVisualisation(
          contextId,
          visualisationId,
          context.Main.idMainX
        )
      )
    )
    context.receive shouldBe Some(
      Api.Response(requestId, Api.VisualisationDetached())
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receive should contain(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId))
    )

    // recompute invalidating x
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(
            Api.InvalidatedExpressions.Expressions(Vector(context.Main.idMainX))
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.Main.Update.mainX(contextId)
    )
  }

  private def send(msg: ApiRequest): Unit =
    context.send(Api.Request(UUID.randomUUID(), msg))
}
