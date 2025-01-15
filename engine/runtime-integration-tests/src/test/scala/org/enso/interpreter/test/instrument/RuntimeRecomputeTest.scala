package org.enso.interpreter.test.instrument

import org.apache.commons.io.output.TeeOutputStream
import org.enso.common.{LanguageInfo, MethodNames, RuntimeOptions}
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Paths}
import java.util.UUID

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeRecomputeTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String)
      extends InstrumentTestContext(packageName)
      with RuntimeServerTest.TestMain {

    val out: ByteArrayOutputStream    = new ByteArrayOutputStream()
    val logOut: ByteArrayOutputStream = new ByteArrayOutputStream()
    protected val context =
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(
          RuntimeOptions.LOG_LEVEL,
          java.util.logging.Level.WARNING.getName
        )
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_EXECUTION_TIMER, "false")
        .option(RuntimeOptions.STRICT_ERRORS, "false")
        .option(
          RuntimeOptions.DISABLE_IR_CACHES,
          InstrumentTestContext.DISABLE_IR_CACHE
        )
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .option(RuntimeOptions.INTERACTIVE_MODE, "true")
        .option(
          RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
          Paths
            .get("../../test/micro-distribution/component")
            .toFile
            .getAbsolutePath
        )
        .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
        .logHandler(new TeeOutputStream(logOut, System.err))
        .out(new TeeOutputStream(out, System.err))
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()

    lazy val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[EnsoContext]

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
  }

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    context.init()
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  override protected def afterEach(): Unit = {
    if (context != null) {
      context.close()
      context.out.reset()
      context = null
    }
  }

  it should "recompute expressions without invalidation" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(contextId, None, None, Seq())
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
  }

  it should "recompute expressions invalidating all" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(Api.InvalidatedExpressions.All()),
          None,
          Seq()
        )
      )
    )
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.pending(
        contextId,
        context.Main.idMainX,
        context.Main.idMainY,
        context.Main.idMainZ,
        context.Main.idFooY,
        context.Main.idFooZ
      ),
      context.Main.Update.mainX(contextId, typeChanged = false),
      context.Main.Update.mainY(contextId, typeChanged = false),
      context.Main.Update.mainZ(contextId, typeChanged = false),
      context.executionComplete(contextId)
    )
  }

  it should "recompute expressions invalidating some" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )
    context.receiveNone shouldEqual None
    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(
            Api.InvalidatedExpressions.Expressions(Vector(context.Main.idMainZ))
          ),
          None,
          Seq()
        )
      )
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.pending(contextId, context.Main.idMainZ),
      context.Main.Update.mainZ(contextId, typeChanged = false),
      context.executionComplete(contextId)
    )
  }

  it should "recompute expressions changing an execution environment" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(contents)
    val moduleName = "Enso_Test.Test.Main"
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, moduleName, "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // recompute
    context.languageContext.getGlobalExecutionEnvironment.getName shouldEqual Api.ExecutionEnvironment
      .Design()
      .name
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          Some(Api.InvalidatedExpressions.All()),
          Some(Api.ExecutionEnvironment.Live()),
          Seq()
        )
      )
    )
    context.receiveN(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.pending(
        contextId,
        context.Main.idMainX,
        context.Main.idMainY,
        context.Main.idMainZ,
        context.Main.idFooY,
        context.Main.idFooZ
      ),
      context.Main.Update.mainX(contextId, typeChanged = false),
      context.Main.Update.mainY(contextId, typeChanged = false),
      context.Main.Update.mainZ(contextId, typeChanged = false),
      context.executionComplete(contextId)
    )
    context.languageContext.getGlobalExecutionEnvironment.getName shouldEqual Api.ExecutionEnvironment
      .Design()
      .name
  }

  it should "recompute expression with expression configs" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val metadata = new Metadata
    val idOut    = metadata.addItem(104, 17, "aa")
    val idIn     = metadata.addItem(131, 16, "ab")

    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Input, Output
        |
        |main =
        |    out = Output.is_enabled
        |    in = Input.is_enabled
        |    IO.println out
        |    IO.println in
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    // Create a new file
    val mainFile = context.writeMain(contents)

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, moduleName, "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.BOOLEAN,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("False", "False")

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idOut, Some(Api.ExecutionEnvironment.Live()))
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.BOOLEAN,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("True", "False")

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idIn, Some(Api.ExecutionEnvironment.Live()))
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("True", "True")
  }

  it should "recompute recursive method call with expression configs" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val metadata = new Metadata
    val idOut    = metadata.addItem(104, 5, "aa")
    val idIn     = metadata.addItem(119, 16, "ab")

    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Input, Output
        |
        |main =
        |    out = fac 3
        |    in = Input.is_enabled
        |    IO.println out
        |    IO.println in
        |
        |fac n=1 acc=1 =
        |    if Output.is_enabled.not then Nothing else
        |        if n <= 0 then acc else
        |            IO.println n
        |            @Tail_Call fac n-1 acc*n
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    // Create a new file
    val mainFile = context.writeMain(contents)

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, moduleName, "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.NOTHING,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              moduleName,
              moduleName,
              "fac"
            ),
            Vector(1)
          )
        )
      ),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Nothing", "False")

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idOut, Some(Api.ExecutionEnvironment.Live()))
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.INTEGER,
        fromCache   = false,
        typeChanged = true,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              moduleName,
              moduleName,
              "fac"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("3", "2", "1", "6", "False")
  }

  it should "recompute method call returning Panic with expression configs" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val metadata = new Metadata
    val idOut    = metadata.addItem(104, 34, "aa")
    val idIn     = metadata.addItem(148, 16, "ab")

    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Input, Output
        |
        |main =
        |    out = Panic.catch Any (foo 42) _.payload
        |    in = Input.is_enabled
        |    IO.println out
        |    IO.println in
        |
        |foo n=Nothing =
        |    Output.if_enabled (Panic.throw n)
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    // Create a new file
    val mainFile = context.writeMain(contents)

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, moduleName, "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        "Standard.Base.Errors.Common.Forbidden_Operation",
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Panic",
              "Standard.Base.Panic.Panic",
              "catch"
            )
          )
        )
      ),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List(
      "(Forbidden_Operation.Error 'The Output context is disabled.')",
      "False"
    )

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idOut, Some(Api.ExecutionEnvironment.Live()))
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.INTEGER,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Panic",
              "Standard.Base.Panic.Panic",
              "catch"
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42", "False")
  }

  it should "recompute expression dropping the cache by providing empty expression configs" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    val metadata = new Metadata
    val idOut    = metadata.addItem(104, 17, "aa")
    val idIn     = metadata.addItem(131, 16, "ab")
    val idOutTxt = metadata.addItem(162, 11, "ac")
    val idInTxt  = metadata.addItem(187, 10, "ad")

    val code =
      """from Standard.Base import all
        |from Standard.Base.Runtime.Context import Input, Output
        |
        |main =
        |    out = Output.is_enabled
        |    in = Input.is_enabled
        |    out_txt = out.to_text
        |    in_txt = in.to_text
        |    IO.println out_txt
        |    IO.println in_txt
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)

    // Create a new file
    val mainFile = context.writeMain(contents)

    // Set sources for the module
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )
    context.consumeOut shouldEqual List()

    // Push new item on the stack to trigger the re-execution
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem
            .ExplicitCall(
              Api.MethodPointer(moduleName, moduleName, "main"),
              None,
              Vector()
            )
        )
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.BOOLEAN,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Runtime",
            "Standard.Base.Runtime.Context",
            "is_enabled"
          ),
          Vector(1)
        )
      ),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Runtime",
            "Standard.Base.Runtime.Context",
            "is_enabled"
          ),
          Vector(1)
        )
      ),
      TestMessages.update(
        contextId,
        idOutTxt,
        ConstantsGen.TEXT,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Any",
            "Standard.Base.Any.Any",
            "to_text"
          ),
          Vector()
        )
      ),
      TestMessages.update(
        contextId,
        idInTxt,
        ConstantsGen.TEXT,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Any",
            "Standard.Base.Any.Any",
            "to_text"
          ),
          Vector()
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("False", "False")

    // recompute
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idIn, Some(Api.ExecutionEnvironment.Live())),
            Api.ExpressionConfig(idOut, Some(Api.ExecutionEnvironment.Live()))
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      TestMessages.update(
        contextId,
        idOut,
        ConstantsGen.BOOLEAN,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      TestMessages.update(
        contextId,
        idInTxt,
        ConstantsGen.TEXT,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Any",
              "Standard.Base.Any.Any",
              "to_text"
            ),
            Vector()
          )
        )
      ),
      TestMessages.update(
        contextId,
        idOutTxt,
        ConstantsGen.TEXT,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Any",
              "Standard.Base.Any.Any",
              "to_text"
            ),
            Vector()
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("True", "True")

    // recompute dropping the caches
    context.send(
      Api.Request(
        requestId,
        Api.RecomputeContextRequest(
          contextId,
          None,
          None,
          Seq(
            Api.ExpressionConfig(idIn, None)
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      TestMessages.update(
        contextId,
        idIn,
        ConstantsGen.BOOLEAN,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Runtime",
              "Standard.Base.Runtime.Context",
              "is_enabled"
            ),
            Vector(1)
          )
        )
      ),
      TestMessages.update(
        contextId,
        idInTxt,
        ConstantsGen.TEXT,
        fromCache   = false,
        typeChanged = false,
        methodCall = Some(
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Any",
              "Standard.Base.Any.Any",
              "to_text"
            ),
            Vector()
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("True", "False")
  }
}
