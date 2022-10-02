package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.instrument.execution.Timer
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.runtime.{Context => EnsoContext}
import org.enso.interpreter.test.Metadata
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeVisualizationsTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  // === Test Timer ===========================================================

  class TestTimer extends Timer {
    override def getTime(): Long = 0
  }

  // === Test Utilities =======================================================

  var context: TestContext = _

  class TestContext(packageName: String) extends InstrumentTestContext {

    val tmpDir: Path = Files.createTempDirectory("enso-test-packages")
    sys.addShutdownHook(FileSystem.removeDirectoryIfExists(tmpDir))
    val lockManager = new ThreadSafeFileLockManager(tmpDir.resolve("locks"))
    val runtimeServerEmulator =
      new RuntimeServerEmulator(messageQueue, lockManager)

    val pkg: Package[File] =
      PackageManager.Default.create(tmpDir.toFile, packageName, "Enso_Test")
    val out: ByteArrayOutputStream    = new ByteArrayOutputStream()
    val logOut: ByteArrayOutputStream = new ByteArrayOutputStream()
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.LOG_LEVEL, "WARNING")
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
        .option(RuntimeServerInfo.ENABLE_OPTION, "true")
        .option(RuntimeOptions.INTERACTIVE_MODE, "true")
        .option(
          RuntimeOptions.DISABLE_IR_CACHES,
          InstrumentTestContext.DISABLE_IR_CACHE
        )
        .option(
          RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
          Paths.get("../../distribution/component").toFile.getAbsolutePath
        )
        .logHandler(logOut)
        .out(out)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
        .build()
    )
    executionContext.context.initialize(LanguageInfo.ID)

    val languageContext = executionContext.context
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[EnsoContext]
    languageContext.getLanguage.getIdExecutionService.ifPresent(
      _.overrideTimer(new TestTimer)
    );

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

    // === The Tests ==========================================================

    object Main {

      val metadata = new Metadata

      val idMainX = metadata.addItem(63, 1)
      val idMainY = metadata.addItem(73, 7)
      val idMainZ = metadata.addItem(89, 5)
      val idFooY  = metadata.addItem(133, 8)
      val idFooZ  = metadata.addItem(150, 5)

      def code =
        metadata.appendToCode(
          """
            |from Standard.Base.Data.Numbers import Number
            |
            |main =
            |    x = 6
            |    y = x.foo 5
            |    z = y + 5
            |    z
            |
            |Number.foo self = x ->
            |    y = self + 3
            |    z = y * x
            |    z
            |""".stripMargin.linesIterator.mkString("\n")
        )

      object Update {

        def mainX(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idMainX,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def mainY(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idMainY,
                  Some(ConstantsGen.INTEGER),
                  Some(
                    Api.MethodPointer(
                      "Enso_Test.Test.Main",
                      ConstantsGen.NUMBER,
                      "foo"
                    )
                  ),
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def mainZ(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idMainZ,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def fooY(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idFooY,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )

        def fooZ(contextId: UUID, fromCache: Boolean = false): Api.Response =
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  Main.idFooZ,
                  Some(ConstantsGen.INTEGER),
                  None,
                  Vector(Api.ProfilingInfo.ExecutionTime(0)),
                  fromCache,
                  Api.ExpressionUpdate.Payload.Value()
                )
              )
            )
          )
      }
    }

    object Visualisation {

      val metadata = new Metadata

      val code =
        metadata.appendToCode(
          """
            |encode x = x.to_text
            |
            |incAndEncode x =
            |    y = x + 1
            |    encode y
            |
            |""".stripMargin.linesIterator.mkString("\n")
        )

    }

    object AnnotatedVisualisation {

      val metadata    = new Metadata
      val idIncY      = metadata.addItem(111, 7)
      val idIncRes    = metadata.addItem(129, 8)
      val idIncMethod = metadata.addItem(102, 43)

      val code =
        metadata.appendToCode(
          """import Standard.Base.IO
            |
            |encode x =
            |   IO.println "encoding..."
            |   x.to_text
            |
            |incAndEncode x a=1 b=1 =
            |    y = a*x + b
            |    res = encode y
            |    res
            |""".stripMargin.linesIterator.mkString("\n")
        )

    }

  }

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
  }

  it should "emit visualisation update when expression is computed" in {
    val idMainRes  = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code
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

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMainRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMainRes,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> encode x"
            )
          )
        )
      )
    )
    val attachVisualisationResponses =
      context.receiveNIgnoreExpressionUpdates(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data.sameElements("50".getBytes) shouldBe true

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )

    val recomputeResponses = context.receiveNIgnoreExpressionUpdates(3)
    recomputeResponses should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    val Some(data2) = recomputeResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data2.sameElements("50".getBytes) shouldBe true
  }

  it should "emit visualisation update when expression is cached" in {
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code
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

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
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
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> encode x"
            )
          )
        )
      )
    )
    val attachVisualisationResponses = context.receiveN(2)
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
    context.receiveNIgnoreExpressionUpdates(2) should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
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
    val recomputeResponses2 = context.receiveNIgnoreExpressionUpdates(3)
    recomputeResponses2 should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
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

  it should "emit visualisation update when expression is modified" in {
    val contents   = context.Main.code
    val moduleName = "Enso_Test.Test.Main"
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
          context.Visualisation.code
        )
      )
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )

    System.out.println("Begin check")
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
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
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> encode x"
            )
          )
        )
      )
    )
    val attachVisualisationResponses = context.receiveN(2)
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

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 9)),
              "5"
            )
          ),
          execute = true
        )
      )
    )

    val editFileResponse = context.receiveNIgnoreExpressionUpdates(2)
    editFileResponse should contain(
      context.executionComplete(contextId)
    )
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
    data1.sameElements("5".getBytes) shouldBe true
  }

  it should "emit visualisation update when transitive expression is modified" in {
    val contents   = context.Main.code
    val moduleName = "Enso_Test.Test.Main"
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
          context.Visualisation.code
        )
      )
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )

    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
    )

    // attach visualization
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          context.Main.idMainZ,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "encode"
            )
          )
        )
      )
    )
    val attachVisualisationResponses = context.receiveN(2)
    attachVisualisationResponses should contain(
      Api.Response(requestId, Api.VisualisationAttached())
    )
    val expectedExpressionId = context.Main.idMainZ
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
    data.sameElements("50".getBytes) shouldBe true

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(4, 8), model.Position(4, 9)),
              "5"
            )
          ),
          execute = true
        )
      )
    )

    val editFileResponse = context.receiveNIgnoreExpressionUpdates(2)
    editFileResponse should contain(
      context.executionComplete(contextId)
    )
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
    data1.sameElements("45".getBytes) shouldBe true
  }

  it should "be able to modify visualisations" in {
    val contents = context.Main.code
    val mainFile = context.writeMain(contents)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    // open files
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code
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
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
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
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> encode x"
            )
          )
        )
      )
    )

    val attachVisualisationResponses = context.receiveN(2)
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

    // modify visualisation
    context.send(
      Api.Request(
        requestId,
        Api.ModifyVisualisation(
          visualisationId,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> incAndEncode x"
            )
          )
        )
      )
    )
    val modifyVisualisationResponses = context.receiveN(2)
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

  it should "not emit visualisation update when visualisation is detached" in {
    val contents = context.Main.code
    val mainFile = context.writeMain(contents)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    // open files
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code
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
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> encode x"
            )
          )
        )
      )
    )
    context.receiveN(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.VisualisationAttached()),
      Api.Response(
        Api.ExecutionFailed(
          contextId,
          Api.ExecutionResult.Failure("Execution stack is empty.", None)
        )
      ),
      context.executionComplete(contextId)
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    val pushResponses = context.receiveNIgnorePendingExpressionUpdates(6)
    pushResponses should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
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
    data.sameElements("6".getBytes) shouldBe true

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
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
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
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
  }

  it should "not emit visualisation update when expression is not affected by the change" in {
    val contents   = context.Main.code
    val moduleName = "Enso_Test.Test.Main"
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
          context.Visualisation.code
        )
      )
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )

    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
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
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "encode"
            )
          )
        )
      )
    )
    val attachVisualisationResponses = context.receiveN(2)
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

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(6, 12), model.Position(6, 13)),
              "6"
            )
          ),
          execute = true
        )
      )
    )

    context.receiveNIgnoreExpressionUpdates(
      1
    ) should contain theSameElementsAs Seq(
      context.executionComplete(contextId)
    )
  }

  it should "not reorder visualization commands" in {
    val contents = context.Main.code
    val mainFile = context.writeMain(contents)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    // open files
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code
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
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      context.executionComplete(contextId)
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
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> encode x"
            )
          )
        )
      )
    )

    val attachVisualisationResponses = context.receiveN(2)
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

    // modify visualisation
    context.send(
      Api.Request(
        requestId,
        Api.ModifyVisualisation(
          visualisationId,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "x -> incAndEncode x"
            )
          )
        )
      )
    )
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
    val modifyVisualisationResponses = context.receiveN(3)
    modifyVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationModified()),
      Api.Response(requestId, Api.VisualisationDetached())
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

  it should "return ModuleNotFound error when attaching visualisation" in {
    val idMain     = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Test.Undefined",
              "x -> x"
            )
          )
        )
      )
    )
    context.receiveN(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.ModuleNotFound("Test.Undefined"))
    )
  }

  it should "be able to use external libraries if they are needed by the visualisation" in {
    val idMain     = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Standard.Visualization.Id",
              "x -> x.default_visualization.to_text"
            )
          )
        )
      )
    )

    val attachVisualisationResponses = context.receiveN(6)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )

    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMain`
              ),
              data
            )
          ) =>
        data
    }

    data.sameElements("(Builtin 'JSON')".getBytes) shouldBe true

    val loadedLibraries = attachVisualisationResponses
      .collect {
        case Api.Response(None, Api.LibraryLoaded(namespace, name, _, _)) =>
          Some((namespace, name))
        case _ => None
      }
      .filter(_.isDefined)
      .flatten

    loadedLibraries should contain(("Standard", "Visualization"))
  }

  it should "return VisualisationExpressionFailed error when attaching visualisation" in {
    val idMain     = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Main",
              "Main.does_not_exist"
            )
          )
        )
      )
    )
    context.receiveN(1) should contain theSameElementsAs Seq(
      Api.Response(
        requestId,
        Api.VisualisationExpressionFailed(
          "Method `does_not_exist` of Main could not be found.",
          Some(
            Api.ExecutionResult.Diagnostic.error(
              message = "Method `does_not_exist` of Main could not be found.",
              stack = Vector(
                Api.StackTraceElement("<eval>", None, None, None),
                Api.StackTraceElement("Debug.eval", None, None, None)
              )
            )
          )
        )
      )
    )
  }

  it should "return visualisation evaluation errors with diagnostic info" in {
    val idMain     = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"

    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              moduleName,
              "x -> x.visualise_me"
            )
          )
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.VisualisationAttached()),
      Api.Response(
        Api.VisualisationEvaluationFailed(
          contextId,
          visualisationId,
          idMain,
          "Method `visualise_me` of 50 (Integer) could not be found.",
          Some(
            Api.ExecutionResult.Diagnostic.error(
              "Method `visualise_me` of 50 (Integer) could not be found.",
              None,
              Some(model.Range(model.Position(0, 5), model.Position(0, 19))),
              None,
              Vector(
                Api.StackTraceElement(
                  "<anonymous>",
                  None,
                  Some(
                    model.Range(model.Position(0, 5), model.Position(0, 19))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "return visualisation error with a stack trace" in {
    val idMain     = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"
    val visualisationCode =
      """
        |encode x = x.visualise_me
        |
        |inc_and_encode x = encode x+1
        |""".stripMargin.linesIterator.mkString("\n")

    val visualisationFile =
      context.writeInSrcDir("Visualisation", visualisationCode)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          visualisationCode
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

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMain, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              "Enso_Test.Test.Visualisation",
              "inc_and_encode"
            )
          )
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.VisualisationAttached()),
      Api.Response(
        Api.VisualisationEvaluationFailed(
          contextId,
          visualisationId,
          idMain,
          "Method `visualise_me` of 51 (Integer) could not be found.",
          Some(
            Api.ExecutionResult.Diagnostic.error(
              "Method `visualise_me` of 51 (Integer) could not be found.",
              Some(visualisationFile),
              Some(model.Range(model.Position(1, 11), model.Position(1, 25))),
              None,
              Vector(
                Api.StackTraceElement(
                  "Visualisation.encode",
                  Some(visualisationFile),
                  Some(
                    model.Range(model.Position(1, 11), model.Position(1, 25))
                  ),
                  None
                ),
                Api.StackTraceElement(
                  "Visualisation.inc_and_encode",
                  Some(visualisationFile),
                  Some(
                    model.Range(model.Position(3, 19), model.Position(3, 29))
                  ),
                  None
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "run visualisation expression catching error" in {
    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()
    val moduleName      = "Enso_Test.Test.Main"
    val metadata        = new Metadata

    val idMain = metadata.addItem(42, 14)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    Error.throw 42
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
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    val responses = context.receiveN(n = 5, timeoutSeconds = 60)

    responses should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        idMain,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(idMain))
      ),
      context.executionComplete(contextId)
    )

    val loadedLibraries = responses.collect {
      case Api.Response(None, Api.LibraryLoaded(namespace, name, _, _)) =>
        (namespace, name)
    }
    loadedLibraries should contain(("Standard", "Base"))

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              moduleName,
              "x -> x.catch_primitive _.to_text"
            )
          )
        )
      )
    )
    val attachVisualisationResponses = context.receiveN(5)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMain`
              ),
              data
            )
          ) =>
        data
    }
    data.sameElements("42".getBytes) shouldBe true
  }

  it should "run visualisation expression propagating panic" in {
    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()
    val moduleName      = "Enso_Test.Test.Main"
    val metadata        = new Metadata

    val idMain = metadata.addItem(42, 14)

    val code =
      """from Standard.Base import all
        |
        |main =
        |    Panic.throw 42
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
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.panic(
        contextId,
        idMain,
        Api.ExpressionUpdate.Payload.Panic("42 (Integer)", Seq(idMain))
      ),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.Text(
              moduleName,
              "x -> Panic.catch_primitive x caught_panic-> caught_panic.payload.to_text"
            )
          )
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      4
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.VisualisationAttached()),
      TestMessages.panic(
        contextId,
        idMain,
        Api.ExpressionUpdate.Payload.Panic("42 (Integer)", Seq(idMain))
      ),
      Api.Response(
        Api.VisualisationEvaluationFailed(
          contextId,
          visualisationId,
          idMain,
          "42 (Integer)",
          Some(
            Api.ExecutionResult.Diagnostic.error(
              message = "42 (Integer)",
              file    = Some(mainFile),
              location =
                Some(model.Range(model.Position(3, 4), model.Position(3, 18))),
              expressionId = Some(idMain),
              stack = Vector(
                Api.StackTraceElement(
                  "Main.main",
                  Some(mainFile),
                  Some(
                    model.Range(model.Position(3, 4), model.Position(3, 18))
                  ),
                  Some(idMain)
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "run internal IDE visualisation preprocessor catching error" in {
    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualisationId = UUID.randomUUID()
    val moduleName      = "Enso_Test.Test.Main"
    val metadata        = new Metadata

    val idMain = metadata.addItem(116, 28)

    val code =
      """import Standard.Base.Data.List
        |import Standard.Visualization
        |from Standard.Base.Error.Common import all
        |
        |main =
        |    Error.throw List.Empty_Error
        |""".stripMargin.linesIterator.mkString("\n")
    val contents = metadata.appendToCode(code)
    val mainFile = context.writeMain(contents)

    // NOTE: below values need to be kept in sync with what is used internally by Rust IDE code
    val visualisationModule   = "Standard.Visualization.Preprocessor"
    val visualisationFunction = "error_preprocessor"

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    val pushContextResponses = context.receiveNIgnorePendingExpressionUpdates(3)
    pushContextResponses should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.error(
        contextId,
        idMain,
        Api.ExpressionUpdate.Payload.DataflowError(Seq(idMain))
      ),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMain,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.ModuleMethod(
              Api.MethodPointer(
                visualisationModule,
                visualisationModule,
                visualisationFunction
              ),
              Vector()
            )
          )
        )
      )
    )
    val attachVisualisationResponses =
      context.receiveNIgnoreExpressionUpdates(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMain`
              ),
              data
            )
          ) =>
        data
    }
    val stringified = new String(data)
    stringified shouldEqual """{ "kind": "Dataflow", "message": "The List is empty."}"""
  }

  it should "attach method pointer visualisation without arguments" in {
    val idMainRes = context.Main.metadata.addItem(99, 1)
    val contents  = context.Main.code
    val mainFile  = context.writeMain(context.Main.code)
    val visualisationFile =
      context.writeInSrcDir("Visualisation", context.Visualisation.code)

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.Visualisation.code
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

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMainRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMainRes,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.ModuleMethod(
              Api.MethodPointer(
                "Enso_Test.Test.Visualisation",
                "Enso_Test.Test.Visualisation",
                "incAndEncode"
              ),
              Vector()
            )
          )
        )
      )
    )
    val attachVisualisationResponses =
      context.receiveNIgnoreExpressionUpdates(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data.sameElements("51".getBytes) shouldBe true

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )

    val recomputeResponses = context.receiveNIgnoreExpressionUpdates(3)
    recomputeResponses should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    val Some(data2) = recomputeResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data2.sameElements("51".getBytes) shouldBe true
  }

  it should "attach method pointer visualisation with arguments" in {
    val idMainRes  = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"
    val visualisationFile =
      context.writeInSrcDir(
        "Visualisation",
        context.AnnotatedVisualisation.code
      )

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.AnnotatedVisualisation.code
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

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMainRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMainRes,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.ModuleMethod(
              Api.MethodPointer(
                "Enso_Test.Test.Visualisation",
                "Enso_Test.Test.Visualisation",
                "incAndEncode"
              ),
              Vector("2", "3")
            )
          )
        )
      )
    )
    val attachVisualisationResponses =
      context.receiveNIgnoreExpressionUpdates(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data.sameElements("103".getBytes) shouldBe true
    context.consumeOut shouldEqual List("encoding...")

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )

    val recomputeResponses = context.receiveNIgnoreExpressionUpdates(3)
    recomputeResponses should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    val Some(data2) = recomputeResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data2.sameElements("103".getBytes) shouldBe true
    context.consumeOut shouldEqual List()

    // modify visualisation
    context.send(
      Api.Request(
        requestId,
        Api.ModifyVisualisation(
          visualisationId,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.ModuleMethod(
              Api.MethodPointer(
                "Enso_Test.Test.Visualisation",
                "Enso_Test.Test.Visualisation",
                "incAndEncode"
              ),
              Vector("2", "4")
            )
          )
        )
      )
    )
    val modifyVisualisationResponses =
      context.receiveNIgnoreExpressionUpdates(2)
    modifyVisualisationResponses should contain(
      Api.Response(requestId, Api.VisualisationModified())
    )
    val Some(data3) =
      modifyVisualisationResponses.collectFirst {
        case Api.Response(
              None,
              Api.VisualisationUpdate(
                Api.VisualisationContext(
                  `visualisationId`,
                  `contextId`,
                  `idMainRes`
                ),
                data
              )
            ) =>
          data
      }
    data3.sameElements("104".getBytes) shouldBe true
    context.consumeOut shouldEqual List("encoding...")
  }

  it should "cache intermediate visualization expressions" in {
    val idMainRes  = context.Main.metadata.addItem(99, 1)
    val contents   = context.Main.code
    val mainFile   = context.writeMain(context.Main.code)
    val moduleName = "Enso_Test.Test.Main"
    val visualisationFile =
      context.writeInSrcDir(
        "Visualisation",
        context.AnnotatedVisualisation.code
      )

    context.send(
      Api.Request(
        Api.OpenFileNotification(
          visualisationFile,
          context.AnnotatedVisualisation.code
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

    // Open the new file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

    // push main
    val item1 = Api.StackItem.ExplicitCall(
      Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
      None,
      Vector()
    )
    context.send(
      Api.Request(requestId, Api.PushContextRequest(contextId, item1))
    )
    context.receiveNIgnorePendingExpressionUpdates(
      6
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.Main.Update.mainX(contextId),
      context.Main.Update.mainY(contextId),
      context.Main.Update.mainZ(contextId),
      TestMessages.update(contextId, idMainRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List()

    // attach visualisation
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualisation(
          visualisationId,
          idMainRes,
          Api.VisualisationConfiguration(
            contextId,
            Api.VisualisationExpression.ModuleMethod(
              Api.MethodPointer(
                "Enso_Test.Test.Visualisation",
                "Enso_Test.Test.Visualisation",
                "incAndEncode"
              ),
              Vector()
            )
          )
        )
      )
    )
    val attachVisualisationResponses =
      context.receiveNIgnoreExpressionUpdates(3)
    attachVisualisationResponses should contain allOf (
      Api.Response(requestId, Api.VisualisationAttached()),
      context.executionComplete(contextId)
    )
    val Some(data) = attachVisualisationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data.sameElements("51".getBytes) shouldBe true
    context.consumeOut shouldEqual List("encoding...")

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None))
    )

    val recomputeResponses = context.receiveNIgnoreExpressionUpdates(3)
    recomputeResponses should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    val Some(data2) = recomputeResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data2.sameElements("51".getBytes) shouldBe true
    context.consumeOut shouldEqual List()

    // Modify the visualization file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          visualisationFile,
          Seq(
            TextEdit(
              model.Range(model.Position(6, 21), model.Position(6, 22)),
              "2"
            )
          ),
          execute = true
        )
      )
    )

    val editFileResponse = context.receiveNIgnoreExpressionUpdates(2)
    editFileResponse should contain(
      context.executionComplete(contextId)
    )
    val Some(data3) = editFileResponse.collectFirst {
      case Api.Response(
            None,
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                `visualisationId`,
                `contextId`,
                `idMainRes`
              ),
              data
            )
          ) =>
        data
    }
    data3.sameElements("52".getBytes) shouldBe true
    context.consumeOut shouldEqual List("encoding...")
  }
}
