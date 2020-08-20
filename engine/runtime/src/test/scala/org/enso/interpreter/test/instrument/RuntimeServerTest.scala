package org.enso.interpreter.test.instrument

import java.io.{ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util
import java.util.UUID
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeServerTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String) {
    var endPoint: MessageEndpoint = _
    val messageQueue: LinkedBlockingQueue[Api.Response] =
      new LinkedBlockingQueue()

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

    def receive(n: Int): List[Api.Response] = {
      Iterator.continually(receive).take(n).flatten.toList
    }

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

    def executionSuccessful(contextId: UUID): Api.Response =
      Api.Response(Api.ExecutionSuccessful(contextId))

    object Main {

      val metadata = new Metadata

      val idMainX = metadata.addItem(16, 1)
      val idMainY = metadata.addItem(26, 7)
      val idMainZ = metadata.addItem(42, 5)
      val idFooY  = metadata.addItem(81, 8)
      val idFooZ  = metadata.addItem(98, 5)

      def code =
        metadata.appendToCode(
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
            |""".stripMargin.linesIterator.mkString("\n")
        )

      object Update {

        def mainX(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainX,
                  Some("Number"),
                  None
                )
              )
            )
          )

        def mainY(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainY,
                  Some("Number"),
                  Some(Api.MethodPointer("Test.Main", "Number", "foo"))
                )
              )
            )
          )

        def mainZ(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idMainZ,
                  Some("Number"),
                  None
                )
              )
            )
          )

        def fooY(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idFooY,
                  Some("Number"),
                  None
                )
              )
            )
          )

        def fooZ(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  Main.idFooZ,
                  Some("Number"),
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

        def mainY(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  idMainY,
                  Some("Number"),
                  Some(Api.MethodPointer("Test.Main", "Main", "foo"))
                )
              )
            )
          )

        def mainZ(contextId: UUID) =
          Api.Response(
            Api.ExpressionValuesComputed(
              contextId,
              Vector(
                Api.ExpressionValueUpdate(
                  idMainZ,
                  Some("Number"),
                  Some(Api.MethodPointer("Test.Main", "Main", "bar"))
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
    context.writeMain(context.Main.code)
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
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.InvalidStackItemError(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionSuccessful(contextId)
    )

    // push method pointer on top of the non-empty stack
    val invalidExplicitCall = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
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
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              context.Main.idMainY,
              Some("Number"),
              Some(Api.MethodPointer("Test.Main", "Number", "foo"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
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

  it should "send updates from last line" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

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
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainFoo,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "foo"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Number"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "foo",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("a", "Any", false, false, None),
                  Suggestion.Argument("b", "Any", false, false, None)
                ),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
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
  }

  it should "compute side effects correctly from last line" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(23, 30)
    val idMainFoo = metadata.addItem(40, 12)

    val code =
      """foo a b = a + b
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
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainFoo,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "foo"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "foo",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("a", "Any", false, false, None),
                  Suggestion.Argument("b", "Any", false, false, None)
                ),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
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
    context.consumeOut shouldEqual List("3")
  }

  it should "run State getting the initial state" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(7, 41)
    val idMainBar = metadata.addItem(39, 8)

    val code =
      """main = IO.println (State.run Number 42 this.bar)
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
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainBar,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "bar"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                moduleName,
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "bar",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
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
    context.consumeOut shouldEqual List("42")
  }

  it should "run State setting the state" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata  = new Metadata
    val idMain    = metadata.addItem(7, 40)
    val idMainBar = metadata.addItem(38, 8)

    val code =
      """main = IO.println (State.run Number 0 this.bar)
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
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainBar,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "bar"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                moduleName,
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "bar",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
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
    context.consumeOut shouldEqual List("10")
  }

  it should "send updates of a function call" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

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
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainFoo,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "foo"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Number"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "foo",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("a", "Any", false, false, None),
                  Suggestion.Argument("b", "Any", false, false, None)
                ),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
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
  }

  it should "not send updates when the type is not changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val idMain     = context.Main.metadata.addItem(7, 47)
    val idMainUpdate =
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Number"), None))
        )
      )
    val contents = context.Main.code
    val mainFile = context.writeMain(contents)

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
    context.receive(7) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      idMainUpdate,
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                moduleName,
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "foo",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("x", "Any", false, false, None)
                ),
                "Number",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainX),
                moduleName,
                "x",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainY),
                moduleName,
                "y",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainZ),
                moduleName,
                "z",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idFooY),
                moduleName,
                "y",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(7, 17), Suggestion.Position(10, 5))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idFooZ),
                moduleName,
                "z",
                "Any",
                Suggestion.Scope(
                  Suggestion.Position(7, 17),
                  Suggestion.Position(10, 5)
                )
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionSuccessful(contextId)
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              context.Main.idMainY,
              Some("Number"),
              Some(Api.MethodPointer("Test.Main", "Number", "foo"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    // pop main
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId))
    )
  }

  it should "send updates when the type is changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata  = new Metadata
    val idResult  = metadata.addItem(20, 4)
    val idPrintln = metadata.addItem(29, 17)
    val idMain    = metadata.addItem(6, 40)
    val code =
      """main =
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
    context.receive(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idResult, Some("Number"), None))
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idPrintln, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                moduleName,
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(idResult),
                moduleName,
                "result",
                "Any",
                Suggestion.Scope(
                  Suggestion.Position(0, 6),
                  Suggestion.Position(2, 21)
                )
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("1337")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 13), model.Position(1, 17)),
              "\"Hi\""
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idResult, Some("Text"), None))
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("Hi")
  }

  it should "send updates when the method pointer is changed" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(6, 35)
    val idMainA  = metadata.addItem(15, 8)
    val idMainP  = metadata.addItem(28, 12)
    val idPie    = metadata.addItem(45 + 8, 1)
    val idUwu    = metadata.addItem(58 + 8, 1)
    val idHie    = metadata.addItem(71 + 8, 6)
    val idXxx    = metadata.addItem(91 + 8, 1)
    val code =
      """main =
        |    a = 123 + 21
        |    IO.println a
        |
        |Main.pie = 3
        |Main.uwu = 7
        |Main.hie = "hie!"
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
    context.receive(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMainA, Some("Number"), None))
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMainP, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                moduleName,
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idPie),
                moduleName,
                "pie",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idUwu),
                moduleName,
                "uwu",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idHie),
                moduleName,
                "hie",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idXxx),
                moduleName,
                "x",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("y", "Any", false, false, None)
                ),
                "Number",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(idMainA),
                moduleName,
                "a",
                "Any",
                Suggestion.Scope(
                  Suggestion.Position(0, 6),
                  Suggestion.Position(3, 0)
                )
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("144")

    // Edit s/123 + 21/1234.x 4/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 8), model.Position(1, 16)),
              "1234.x 4"
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainA,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "x"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("4")

    // Edit s/1234.x 4/1000.x 5/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 8), model.Position(1, 16)),
              "1000.x 5"
            )
          )
        )
      )
    )
    context.receive shouldEqual Some(context.executionSuccessful(contextId))
    context.consumeOut shouldEqual List("5")

    // Edit s/1000.x 5/Main.pie/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 8), model.Position(1, 16)),
              "Main.pie"
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainA,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "pie"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("3")

    // Edit s/Main.pie/Main.uwu/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 8), model.Position(1, 16)),
              "Main.uwu"
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainA,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Main", "uwu"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("7")

    // Edit s/Main.uwu/Main.hie/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 8), model.Position(1, 16)),
              "Main.hie"
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              idMainA,
              Some("Text"),
              Some(Api.MethodPointer(moduleName, "Main", "hie"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("hie!")

    // Edit s/Main.hie/"Hello!"/
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 8), model.Position(1, 16)),
              "\"Hello!\""
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMainA, Some("Text"), None))
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("Hello!")
  }

  it should "send updates for overloaded functions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata
    val idMain   = metadata.addItem(6, 89)
    val id1      = metadata.addItem(15, 15)
    val id2      = metadata.addItem(35, 18)
    val id3      = metadata.addItem(58, 16)
    val idy      = metadata.addItem(83, 2)
    val code =
      """main =
        |    x = 15.overloaded 1
        |    "foo".overloaded 2
        |    overloaded 10 30
        |    y = 42
        |    Unit
        |
        |Text.overloaded arg = 10
        |Number.overloaded arg = 20
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
    context.receive(8) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Unit"), None))
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id1,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id2,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Text", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id3,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idy, Some("Number"), None))
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                moduleName,
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "overloaded",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("arg", "Any", false, false, None)
                ),
                "Text",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "overloaded",
                Seq(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("arg", "Any", false, false, None)
                ),
                "Number",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(id1),
                moduleName,
                "x",
                "Any",
                Suggestion.Scope(
                  Suggestion.Position(0, 6),
                  Suggestion.Position(6, 0)
                )
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(idy),
                moduleName,
                "y",
                "Any",
                Suggestion.Scope(
                  Suggestion.Position(0, 6),
                  Suggestion.Position(6, 0)
                )
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )

    // pop call1
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id1,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id2,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Text", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id3,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )

    // pop call2
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id1,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id2,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Text", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id3,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )

    // pop call3
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id1,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id2,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Text", "overloaded"))
            )
          )
        )
      ),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              id3,
              Some("Number"),
              Some(Api.MethodPointer(moduleName, "Number", "overloaded"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
  }

  it should "support file modification operations" in {
    val fooFile   = new File(context.pkg.sourceDir, "Foo.enso")
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Create a new file
    context.writeFile(fooFile, "main = IO.println \"I'm a file!\"")

    // Open the new file
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          fooFile,
          "main = IO.println \"I'm a file!\"",
          false
        )
      )
    )
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
              Api.MethodPointer("Test.Foo", "Foo", "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          "Test.Foo",
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                "Test.Foo",
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Foo",
                "Any",
                None
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("I'm a file!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          fooFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 25), model.Position(0, 29)),
              "modified"
            )
          )
        )
      )
    )
    context.receive shouldEqual Some(context.executionSuccessful(contextId))
    context.consumeOut shouldEqual List("I'm a modified!")

    // Close the file
    context.send(Api.Request(Api.CloseFileNotification(fooFile)))
    context.consumeOut shouldEqual List()
  }

  it should "support file modification operations with attached ids" in {
    val fooFile   = new File(context.pkg.sourceDir, "Foo.enso")
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()
    val metadata  = new Metadata
    val idMain    = metadata.addItem(7, 2)
    val code      = metadata.appendToCode("main = 84")

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Create a new file
    context.writeFile(fooFile, code)

    // Open the new file
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          fooFile,
          code,
          false
        )
      )
    )
    context.receiveNone shouldEqual None

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer("Test.Foo", "Foo", "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(idMain, Some("Number"), None)
          )
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          "Test.Foo",
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                "Test.Foo",
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Foo",
                "Any",
                None
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          fooFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 0), model.Position(0, 9)),
              "main = 42"
            )
          )
        )
      )
    )
    context.receive shouldEqual Some(context.executionSuccessful(contextId))
  }

  it should "send suggestion notifications when file is executed" in {
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()
    val idMain    = context.Main.metadata.addItem(7, 47)
    val idMainUpdate =
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(Api.ExpressionValueUpdate(idMain, Some("Number"), None))
        )
      )

    val mainFile = context.writeMain(context.Main.code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          mainFile,
          context.Main.code,
          false
        )
      )
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(7) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      idMainUpdate,
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          "Test.Main",
          List(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                Some(idMain),
                "Test.Main",
                "main",
                List(Suggestion.Argument("this", "Any", false, false, None)),
                "Main",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                "Test.Main",
                "foo",
                List(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("x", "Any", false, false, None)
                ),
                "Number",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainX),
                "Test.Main",
                "x",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainY),
                "Test.Main",
                "y",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainZ),
                "Test.Main",
                "z",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idFooY),
                "Test.Main",
                "y",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(7, 17), Suggestion.Position(10, 5))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idFooZ),
                "Test.Main",
                "z",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(7, 17), Suggestion.Position(10, 5))
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    // push foo call
    val item2 = Api.StackItem.LocalCall(context.Main.idMainY)
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item2))
    )
    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.fooY(contextId),
      context.Main.Update.fooZ(contextId),
      context.executionSuccessful(contextId)
    )

    // pop foo call
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              context.Main.idMainY,
              Some("Number"),
              Some(Api.MethodPointer("Test.Main", "Number", "foo"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    // pop main
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PopContextResponse(contextId))
    )

    // pop empty stack
    context.send(Api.Request(requestId, Api.PopContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.EmptyStackError(contextId))
    )
  }

  it should "send suggestion notifications when file is modified" in {
    val fooFile   = new File(context.pkg.sourceDir, "Foo.enso")
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Create a new file
    context.writeFile(fooFile, "main = IO.println \"I'm a file!\"")

    // Open the new file
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          fooFile,
          "main = IO.println \"I'm a file!\"",
          false
        )
      )
    )
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
              Api.MethodPointer("Test.Foo", "Foo", "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          "Test.Foo",
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                "Test.Foo",
                "main",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Foo",
                "Any",
                None
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("I'm a file!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          fooFile,
          Seq(
            TextEdit(
              model.Range(model.Position(0, 25), model.Position(0, 29)),
              "modified"
            ),
            TextEdit(
              model.Range(model.Position(0, 0), model.Position(0, 0)),
              "Number.lucky = 42\n\n"
            )
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                "Test.Foo",
                "lucky",
                Seq(Suggestion.Argument("this", "Any", false, false, None)),
                "Number",
                "Any",
                None
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("I'm a modified!")

    // Close the file
    context.send(Api.Request(Api.CloseFileNotification(fooFile)))
    context.receiveNone shouldEqual None
    context.consumeOut shouldEqual List()
  }

  it should "recompute expressions without invalidation" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )
  }

  it should "recompute expressions invalidating all" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )
  }

  it should "recompute expressions invalidating some" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )
  }

  it should "return error when module not found" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer("Unnamed.Main", "Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(contextId, "Module Unnamed.Main not found.")
      )
    )
  }

  it should "return error when constructor not found" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer("Test.Main", "Unexpected", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          "Constructor Unexpected not found in module Test.Main."
        )
      )
    )
  }

  it should "return error when method not found" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer("Test.Main", "Main", "ooops"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          "Object Main does not define method ooops in module Test.Main."
        )
      )
    )
  }

  it should "return error not invocable" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata
    val code =
      """main = this.bar 40 2 9
        |
        |bar x y = x + y
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(contextId, "Object 42 is not invokable.")
      )
    )
  }

  it should "return error in function main" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata
    val code =
      """main = this.bar x y
        |
        |bar x y = x + y
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(Api.ExecutionFailed(contextId, "Error in function main."))
    )
  }

  it should "return error unexpected type" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata
    val code =
      """main = this.bar "one" 2
        |
        |bar x y = x + y
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          "Unexpected type provided for argument `that` in Text.+"
        )
      )
    )
  }

  it should "return error method does not exist" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    val code     = "main = Number.pi"
    val contents = metadata.appendToCode(code)
    context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          "Object Number does not define method pi."
        )
      )
    )
  }

  it should "skip side effects when evaluating cached expression" in {
    context.writeMain(context.Main2.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main2.Update.mainY(contextId),
      context.Main2.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List("I'm expensive!", "I'm more expensive!")

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
    )
    context.consumeOut shouldEqual List()
  }

  it should "emit visualisation update when expression is evaluated" in {
    context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code,
          false
        )
      )
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
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )

    // attach visualisation
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
    val attachVisualisationResponses = context.receive(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionSuccessful(contextId)
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
    context.receive(2) should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
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
      context.executionSuccessful(contextId)
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

  it should "emit visualisation update without value update" in {
    val contents   = context.Main.code
    val moduleName = "Test.Main"
    val mainFile   = context.writeMain(contents)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // open files
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code,
          false
        )
      )
    )
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents, false))
    )
    context.receiveNone shouldEqual None

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )

    context.receive(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
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
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Method(
                None,
                moduleName,
                "foo",
                List(
                  Suggestion.Argument("this", "Any", false, false, None),
                  Suggestion.Argument("x", "Any", false, false, None)
                ),
                "Number",
                "Any",
                None
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainX),
                "Test.Main",
                "x",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainY),
                moduleName,
                "y",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idMainZ),
                moduleName,
                "z",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(1, 6), Suggestion.Position(6, 0))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idFooY),
                moduleName,
                "y",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(7, 17), Suggestion.Position(10, 5))
              )
            ),
            Api.SuggestionsDatabaseUpdate.Add(
              Suggestion.Local(
                Some(context.Main.idFooZ),
                moduleName,
                "z",
                "Any",
                Suggestion
                  .Scope(Suggestion.Position(7, 17), Suggestion.Position(10, 5))
              )
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )

    // attach visualization
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
    val attachVisualisationResponses = context.receive(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionSuccessful(contextId)
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
    util.Arrays.equals(data, "6".getBytes) shouldBe true

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 8), model.Position(2, 9)),
              "5"
            )
          )
        )
      )
    )

    val editFileResponse = context.receive(2)
    editFileResponse should contain(context.executionSuccessful(contextId))
    val Some(data1) = editFileResponse.collectFirst {
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
    util.Arrays.equals(data1, "5".getBytes) shouldBe true
  }

  it should "be able to modify visualisations" in {
    context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code,
          false
        )
      )
    )
    context.receiveNone shouldEqual None

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
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )

    // attach visualisation
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

    val attachVisualisationResponses = context.receive(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionSuccessful(contextId)
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

    // modify visualisation
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
    val modifyVisualisationResponses = context.receive(3)
    modifyVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationModified()),
      context.executionSuccessful(contextId)
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
    context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code,
          false
        )
      )
    )

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // attach visualisation
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
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.VisualisationAttached()),
      Api.Response(Api.ExecutionFailed(contextId, "Stack is empty."))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Test.Main", "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    val pushResponses = context.receive(6)
    pushResponses should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )
    val expectedExpressionId = context.Main.idMainX
    val Some(data) =
      pushResponses.collectFirst {
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
    util.Arrays.equals(data, "6".getBytes) shouldEqual true

    // detach visualisation
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
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.VisualisationDetached())
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
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
      context.executionSuccessful(contextId)
    )
  }

  it should "rename a project" in {
    context.writeMain(context.Main.code)
    val contextId = UUID.randomUUID()
    val requestId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer("Test.Main", "Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionSuccessful(contextId)
    )

    // rename Test -> Foo
    context.pkg.rename("Foo")
    context.send(Api.Request(requestId, Api.RenameProject("Test", "Foo")))
    context.receive(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.ProjectRenamed("Foo"))
    )

    // recompute existing stack
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )
    context.receive(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionSuccessful(contextId)
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
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      Api.Response(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(
            Api.ExpressionValueUpdate(
              context.Main.idMainY,
              Some("Number"),
              Some(Api.MethodPointer("Foo.Main", "Number", "foo"))
            )
          )
        )
      ),
      context.executionSuccessful(contextId)
    )
  }

}
