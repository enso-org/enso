package org.enso.interpreter.test.instrument

import java.io.{ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import org.enso.interpreter.instrument.execution.Timer
import org.enso.interpreter.runtime.`type`.Constants
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.{ContentVersion, Sha3_224VersionCalculator}
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeInstrumentTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  // === Test Timer ===========================================================

  class TestTimer extends Timer {
    override def getTime(): Long = 0
  }

  // === Test Utilities =======================================================

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
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
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

    val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[org.enso.interpreter.runtime.Context]
    languageContext.getLanguage.getIdExecutionInstrument
      .overrideTimer(new TestTimer)

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
      Option(messageQueue.poll(10, TimeUnit.SECONDS))
    }

    def receive(n: Int): List[Api.Response] = {
      Iterator.continually(receive).take(n).flatten.toList
    }

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

    def executionComplete(contextId: UUID): Api.Response =
      Api.Response(Api.ExecutionComplete(contextId))
  }

  def contentsVersion(content: String): ContentVersion =
    Sha3_224VersionCalculator.evalVersion(content)

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  it should "instrument simple expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata
    val mainBody = metadata.addItem(7, 2)

    val code     = "main = 42"
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, mainBody, Constants.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "instrument default hello world example" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata
    val mainBody = metadata.addItem(33, 14)

    val code =
      """from Builtins import all
        |
        |main = "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, mainBody, Constants.TEXT),
      context.executionComplete(contextId)
    )
  }

  it should "instrument expressions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata    = new Metadata
    val mainBody    = metadata.addItem(32, 52)
    val xExpr       = metadata.addItem(41, 2)
    val yExpr       = metadata.addItem(52, 5)
    val zExpr       = metadata.addItem(66, 1)
    val mainResExpr = metadata.addItem(72, 12)

    val code =
      """from Builtins import all
        |
        |main =
        |    x = 42
        |    y = x + 1
        |    z = y
        |    IO.println z
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(7) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xExpr, Constants.INTEGER),
      TestMessages.update(contextId, yExpr, Constants.INTEGER),
      TestMessages.update(contextId, zExpr, Constants.INTEGER),
      TestMessages.update(contextId, mainResExpr, Constants.NOTHING),
      TestMessages.update(contextId, mainBody, Constants.NOTHING),
      context.executionComplete(contextId)
    )
  }

  it should "instrument sub-expressions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata     = new Metadata
    val mainBody     = metadata.addItem(32, 42)
    val xExpr        = metadata.addItem(41, 2)
    val yExpr        = metadata.addItem(52, 5)
    val mainResExpr  = metadata.addItem(62, 12)
    val mainRes1Expr = metadata.addItem(73, 1)

    val code =
      """from Builtins import all
        |
        |main =
        |    x = 42
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

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(7) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xExpr, Constants.INTEGER),
      TestMessages.update(contextId, yExpr, Constants.INTEGER),
      TestMessages.update(contextId, mainRes1Expr, Constants.INTEGER),
      TestMessages.update(contextId, mainResExpr, Constants.NOTHING),
      TestMessages.update(contextId, mainBody, Constants.NOTHING),
      context.executionComplete(contextId)
    )
  }

  it should "instrument binding of a lambda" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata = new Metadata
    val mainBody = metadata.addItem(32, 28)
    val fExpr    = metadata.addItem(41, 10)
    // f body
    metadata.addItem(46, 5)
    val mainResExpr = metadata.addItem(56, 4)

    val code =
      """from Builtins import all
        |
        |main =
        |    f = x -> x + 1
        |    f 42
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, fExpr, Constants.FUNCTION),
      TestMessages.update(contextId, mainResExpr, Constants.INTEGER),
      TestMessages.update(contextId, mainBody, Constants.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "instrument binding of sugared lambda" in {
    pending
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"

    val metadata    = new Metadata
    val fExpr       = metadata.addItem(41, 5)
    val xExpr       = metadata.addItem(55, 4)
    val mainResExpr = metadata.addItem(64, 1)

    val code =
      """from Builtins import all
        |
        |main =
        |    f = _ + 1
        |    x = f 42
        |    x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(6) should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, fExpr, Constants.FUNCTION),
      TestMessages.update(contextId, xExpr, Constants.INTEGER),
      TestMessages.update(contextId, mainResExpr, Constants.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument the body of a method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    // f expression
    metadata.addItem(32, 5)
    val xExpr    = metadata.addItem(54, 8)
    val mainRes  = metadata.addItem(67, 1)
    val mainExpr = metadata.addItem(45, 23)

    val code =
      """from Builtins import all
        |
        |f x = x + 1
        |
        |main =
        |    x = here.f 1
        |    x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages
        .update(
          contextId,
          xExpr,
          Constants.INTEGER,
          Api.MethodPointer("Test.Main", "Test.Main", "f")
        ),
      TestMessages.update(contextId, mainRes, Constants.INTEGER),
      TestMessages.update(contextId, mainExpr, Constants.INTEGER),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument the body of a function" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    // f expression
    metadata.addItem(43, 5)
    val aExpr    = metadata.addItem(57, 1)
    val fApp     = metadata.addItem(75, 3)
    val mainRes  = metadata.addItem(63, 16)
    val mainExpr = metadata.addItem(32, 47)

    val code =
      """from Builtins import all
        |
        |main =
        |    f x = x + 1
        |    a = 1
        |    IO.println (f a)
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, aExpr, Constants.INTEGER),
      TestMessages.update(contextId, fApp, Constants.INTEGER),
      TestMessages.update(contextId, mainRes, Constants.NOTHING),
      TestMessages.update(contextId, mainExpr, Constants.NOTHING),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument the body of a lambda" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    val aExpr = metadata.addItem(41, 14)
    // lambda
    metadata.addItem(42, 10)
    // lambda expression
    metadata.addItem(47, 5)
    val lamArg  = metadata.addItem(54, 1)
    val mainRes = metadata.addItem(60, 12)

    val code =
      """from Builtins import all
        |
        |main =
        |    a = (x -> x + 1) 1
        |    IO.println a
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, aExpr, Constants.INTEGER),
      TestMessages.update(contextId, lamArg, Constants.INTEGER),
      TestMessages.update(contextId, mainRes, Constants.NOTHING),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument the body of a sugared lambda" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    val aExpr = metadata.addItem(41, 9)
    // lambda
    metadata.addItem(42, 5)
    val lamArg  = metadata.addItem(49, 1)
    val mainRes = metadata.addItem(55, 12)

    val code =
      """from Builtins import all
        |
        |main =
        |    a = (_ + 1) 1
        |    IO.println a
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, aExpr, Constants.INTEGER),
      TestMessages.update(contextId, lamArg, Constants.INTEGER),
      TestMessages.update(contextId, mainRes, Constants.NOTHING),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("2")
  }

  it should "not instrument functions in block expressions" in {
    pending
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    val xExpr = metadata.addItem(40, 33)
    // function body
    metadata.addItem(55, 5)
    // x result
    metadata.addItem(69, 4)
    val mainRes = metadata.addItem(78, 1)

    val code =
      """from Builtins import all
        |
        |main =
        |    x =
        |        f x = x + 1
        |        f 42
        |    x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xExpr, Constants.THUNK),
      TestMessages.update(contextId, mainRes, Constants.THUNK),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument lambdas in block expressions" in {
    pending
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    val xExpr = metadata.addItem(40, 36)
    // lambda
    metadata.addItem(53, 10)
    // lambda body
    metadata.addItem(58, 5)
    // x result
    metadata.addItem(72, 4)
    val mainRes = metadata.addItem(81, 1)

    val code =
      """from Builtins import all
        |
        |main =
        |    x =
        |        f = x -> x + 1
        |        f 42
        |    x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(4) should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xExpr, Constants.THUNK),
      TestMessages.update(contextId, mainRes, Constants.THUNK),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument sugared lambdas in block expressions" in {
    pending
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    val xExpr = metadata.addItem(40, 31)
    // lambda
    metadata.addItem(53, 5)
    // x result
    metadata.addItem(67, 4)
    val mainRes = metadata.addItem(76, 1)

    val code =
      """from Builtins import all
        |
        |main =
        |    x =
        |        f = _ + 1
        |        f 42
        |    x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, xExpr, Constants.THUNK),
      TestMessages.update(contextId, mainRes, Constants.THUNK),
      context.executionComplete(contextId)
    )
  }

  it should "not instrument a lambda in argument" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Test.Main"
    val metadata   = new Metadata

    // body of id method
    metadata.addItem(42, 1)
    // body of id1 function
    metadata.addItem(78, 3)
    // default lambda argument a->a in id method
    metadata.addItem(34, 4)
    // default lambda argument a->a in id1 function
    metadata.addItem(70, 4)
    // first x->x argument
    metadata.addItem(94, 4)
    // second x->x argument
    metadata.addItem(148, 4)
    val arg1 = metadata.addItem(90, 2)
    val arg2 = metadata.addItem(101, 2)
    val arg3 = metadata.addItem(133, 2)

    val code =
      """from Builtins import all
        |
        |id (x = a->a) = x
        |
        |main =
        |    id1 x=42 (y = a->a) = y x
        |    id1 42
        |    id1 42 x->x
        |    here.id
        |    here.id 42
        |    here.id x->x
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
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
            Api.MethodPointer(moduleName, "Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receive(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, arg1, Constants.INTEGER),
      TestMessages.update(contextId, arg2, Constants.INTEGER),
      TestMessages.update(contextId, arg3, Constants.INTEGER),
      context.executionComplete(contextId)
    )
  }

}
