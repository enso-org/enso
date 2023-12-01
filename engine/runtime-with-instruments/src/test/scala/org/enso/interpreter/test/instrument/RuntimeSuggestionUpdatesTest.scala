package org.enso.interpreter.test.instrument

import org.apache.commons.io.FileUtils
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.data.Tree
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
import java.util.logging.Level
import scala.concurrent.Await
import scala.concurrent.duration._

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeSuggestionUpdatesTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String) extends InstrumentTestContext {

    val tmpDir: Path = Files.createTempDirectory("enso-test-packages")
    val lockManager  = new ThreadSafeFileLockManager(tmpDir.resolve("locks"))
    val runtimeServerEmulator =
      new RuntimeServerEmulator(messageQueue, lockManager)

    val pkg: Package[File] =
      PackageManager.Default.create(tmpDir.toFile, packageName, "Enso_Test")
    val out: ByteArrayOutputStream = new ByteArrayOutputStream()
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName)
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
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
        .out(out)
        .logHandler(System.err)
        .serverTransport(runtimeServerEmulator.makeServerTransport)
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
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive

    context.send(
      Api.Request(UUID.randomUUID(), Api.StartBackgroundProcessing())
    )
    context.receive shouldEqual Some(
      Api.Response(Api.BackgroundJobsStartedNotification())
    )
  }

  override protected def afterEach(): Unit = {
    if (context != null) {
      context.reset()
      context.executionContext.context.close()
      Await.ready(context.runtimeServerEmulator.terminate(), 5.seconds)
      context.lockManager.reset()
      context.out.reset()
      FileUtils.deleteQuietly(context.tmpDir.toFile)
      context = null
    }
  }

  it should "send suggestion updates after file modification" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val code =
      """from Standard.Base import all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val mainFile = context.writeMain(code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, code))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 6), model.Position(2, 32)),
              "\n    x = 42\n    IO.println x"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "x",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(4, 16)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Add()
                    ),
                    Vector()
                  )
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 10), model.Position(3, 10)),
              "\n    y = 9"
            ),
            TextEdit(
              model.Range(model.Position(5, 15), model.Position(5, 16)),
              "x+y"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "x",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(4, 16)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Modify(scope =
                        Some(
                          Suggestion.Scope(
                            Suggestion.Position(2, 6),
                            Suggestion.Position(5, 18)
                          )
                        )
                      )
                    ),
                    Vector()
                  ),
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "y",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(5, 18)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Add()
                    ),
                    Vector()
                  )
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("51")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 10), model.Position(3, 10)),
              "\n    y : Number"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "x",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(5, 18)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Modify(scope =
                        Some(
                          Suggestion.Scope(
                            Suggestion.Position(2, 6),
                            Suggestion.Position(6, 18)
                          )
                        )
                      )
                    ),
                    Vector()
                  ),
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "y",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(5, 18)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Modify(
                        returnType = Some(ConstantsGen.NUMBER),
                        scope = Some(
                          Suggestion.Scope(
                            Suggestion.Position(2, 6),
                            Suggestion.Position(6, 18)
                          )
                        )
                      )
                    ),
                    Vector()
                  )
                )
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("51")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(1, 0), model.Position(1, 0)),
              "\nfoo x = x * 10\n"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "x",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(6, 18)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Modify(scope =
                        Some(
                          Suggestion.Scope(
                            Suggestion.Position(4, 6),
                            Suggestion.Position(8, 18)
                          )
                        )
                      )
                    ),
                    Vector()
                  ),
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "y",
                        ConstantsGen.NUMBER,
                        Suggestion.Scope(
                          Suggestion.Position(2, 6),
                          Suggestion.Position(6, 18)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Modify(scope =
                        Some(
                          Suggestion.Scope(
                            Suggestion.Position(4, 6),
                            Suggestion.Position(8, 18)
                          )
                        )
                      )
                    ),
                    Vector()
                  )
                )
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "foo",
                    List(
                      Suggestion
                        .Argument("x", ConstantsGen.ANY, false, false, None)
                    ),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("51")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 4), model.Position(2, 14)),
              "a b = a * b"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "foo",
                    List(
                      Suggestion
                        .Argument("x", ConstantsGen.ANY, false, false, None)
                    ),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Modify(
                    None,
                    Some(
                      List(
                        Api.SuggestionArgumentAction
                          .Modify(0, Some("a"), None, None, None, None),
                        Api.SuggestionArgumentAction.Add(
                          1,
                          Suggestion
                            .Argument("b", ConstantsGen.ANY, false, false, None)
                        )
                      )
                    ),
                    None,
                    None,
                    None
                  )
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("51")
  }

  it should "send suggestion updates after renaming the project" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val code =
      """from Standard.Base import all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val mainFile = context.writeMain(code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, code))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")

    // rename Test -> Foo
    context.pkg.rename("Foo")
    context.send(
      Api.Request(requestId, Api.RenameProject("Enso_Test", "Test", "Foo"))
    )
    context.receiveN(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.ProjectRenamed("Test", "Foo", "Foo")),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.empty
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = "Enso_Test.Foo.Main",
          actions =
            Vector(Api.SuggestionsDatabaseAction.Clean("Enso_Test.Foo.Main")),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    "Enso_Test.Foo.Main",
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    "Enso_Test.Foo.Main",
                    "main",
                    List(),
                    "Enso_Test.Foo.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")
  }

  it should "index overloaded functions" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    // Note that Text.Text.overloaded is only to ensure that tests match expectations.
    // In general Text.overloaded would also work because method resolution would assign it
    // to the module rather than a type
    val contents =
      """from Standard.Base.Data.Numbers import Number
        |import Standard.Base.Data.Text
        |import Standard.Base.Nothing
        |
        |main =
        |    x = 15.overloaded 1
        |    "foo".overloaded 2
        |    10.overloaded x
        |    Nothing
        |
        |Text.Text.overloaded self arg = arg + 1
        |Number.overloaded self arg = arg + 2
        |""".stripMargin.linesIterator.mkString("\n")
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, contents))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      3
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    Seq(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestion.Local(
                        None,
                        moduleName,
                        "x",
                        ConstantsGen.ANY,
                        Suggestion.Scope(
                          Suggestion.Position(4, 6),
                          Suggestion.Position(8, 11)
                        ),
                        None
                      ),
                      Api.SuggestionAction.Add()
                    ),
                    Vector()
                  )
                )
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "overloaded",
                    Seq(
                      Suggestion.Argument(
                        "self",
                        ConstantsGen.TEXT,
                        false,
                        false,
                        None
                      ),
                      Suggestion
                        .Argument("arg", ConstantsGen.ANY, false, false, None)
                    ),
                    ConstantsGen.TEXT,
                    ConstantsGen.ANY,
                    false,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "overloaded",
                    Seq(
                      Suggestion.Argument(
                        "self",
                        ConstantsGen.NUMBER,
                        false,
                        false,
                        None
                      ),
                      Suggestion
                        .Argument("arg", ConstantsGen.ANY, false, false, None)
                    ),
                    ConstantsGen.NUMBER,
                    ConstantsGen.ANY,
                    false,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      context.executionComplete(contextId)
    )
  }

  it should "index exports" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val mainCode =
      """from Standard.Base import all
        |
        |import Enso_Test.Test.A
        |from Enso_Test.Test.A export all
        |import Enso_Test.Test.A.MyType
        |from Enso_Test.Test.A.MyType export all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val aCode =
      """from Standard.Base import all
        |
        |type MyType
        |    MkA a
        |
        |Integer.fortytwo self = 42
        |
        |hello = "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")

    val mainFile = context.writeMain(mainCode)
    val aFile    = context.writeInSrcDir("A", aCode)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open files
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, mainCode))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(aFile, aCode))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      5
    ) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = "Enso_Test.Test.A",
          actions =
            Vector(Api.SuggestionsDatabaseAction.Clean("Enso_Test.Test.A")),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module("Enso_Test.Test.A", None),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Type(
                    None,
                    "Enso_Test.Test.A",
                    "MyType",
                    List(),
                    "Enso_Test.Test.A.MyType",
                    Some(ConstantsGen.ANY),
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Constructor(
                    None,
                    "Enso_Test.Test.A",
                    "MkA",
                    List(
                      Suggestion
                        .Argument("a", ConstantsGen.ANY, false, false, None)
                    ),
                    "Enso_Test.Test.A.MyType",
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Getter(
                    None,
                    "Enso_Test.Test.A",
                    "a",
                    List(
                      Suggestion
                        .Argument(
                          "self",
                          "Enso_Test.Test.A.MyType",
                          false,
                          false,
                          None
                        )
                    ),
                    "Enso_Test.Test.A.MyType",
                    ConstantsGen.ANY,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    "Enso_Test.Test.A",
                    "fortytwo",
                    List(
                      Suggestion.Argument(
                        "self",
                        ConstantsGen.INTEGER,
                        false,
                        false,
                        None
                      )
                    ),
                    ConstantsGen.INTEGER,
                    ConstantsGen.ANY,
                    false,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    "Enso_Test.Test.A",
                    "hello",
                    Seq(),
                    "Enso_Test.Test.A",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(
            Api.ExportsUpdate(
              ModuleExports(
                "Enso_Test.Test.Main",
                Set(
                  ExportedSymbol.Type("Enso_Test.Test.A", "MyType"),
                  ExportedSymbol.Constructor("Enso_Test.Test.A", "MkA"),
                  ExportedSymbol.Method("Enso_Test.Test.A", "hello")
                )
              ),
              Api.ExportsAction.Add()
            )
          ),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.DefinedMethod(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    true,
                    None,
                    Seq()
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
            )
          )
        )
      ),
      Api.Response(Api.AnalyzeModuleInScopeJobFinished()),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 32), model.Position(3, 32)),
              " hiding hello"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnoreExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(
            Api.ExportsUpdate(
              ModuleExports(
                "Enso_Test.Test.Main",
                Set(ExportedSymbol.Method("Enso_Test.Test.A", "hello"))
              ),
              Api.ExportsAction.Remove()
            )
          ),
          updates = Tree.Root(Vector())
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")

    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(2, 0), model.Position(7, 0)),
              ""
            )
          ),
          execute = true
        )
      )
    )
    context.receiveNIgnorePendingExpressionUpdates(
      2
    ) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          actions = Vector(),
          exports = Vector(
            Api.ExportsUpdate(
              ModuleExports(
                "Enso_Test.Test.Main",
                Set(
                  ExportedSymbol.Type("Enso_Test.Test.A", "MyType"),
                  ExportedSymbol.Constructor("Enso_Test.Test.A", "MkA")
                )
              ),
              Api.ExportsAction.Remove()
            )
          ),
          updates = Tree.Root(Vector())
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("Hello World!")
  }

  it should "invalidate modules index on command" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val code =
      """from Standard.Base import all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val mainFile = context.writeMain(code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(requestId, Api.OpenFileRequest(mainFile, code))
    )
    context.receive shouldEqual Some(
      Api.Response(Some(requestId), Api.OpenFileResponse)
    )

    // push main
    context.send(
      Api.Request(
        requestId,
        Api.PushContextRequest(
          contextId,
          Api.StackItem.ExplicitCall(
            Api.MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
            None,
            Vector()
          )
        )
      )
    )
    val updates1 = context.receiveNIgnoreExpressionUpdates(3)
    updates1.length shouldEqual 3
    updates1 should contain allOf (
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId),
    )
    val indexedModules = updates1.collect {
      case Api.Response(
            None,
            Api.SuggestionsDatabaseModuleUpdateNotification(moduleName, _, _, _)
          ) =>
        moduleName
    }
    indexedModules should contain theSameElementsAs Seq(moduleName)
    context.consumeOut shouldEqual List("Hello World!")

    // clear indexes
    context.send(Api.Request(requestId, Api.InvalidateModulesIndexRequest()))
    context.receiveN(1) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.InvalidateModulesIndexResponse())
    )

    // recompute
    context.send(
      Api.Request(requestId, Api.RecomputeContextRequest(contextId, None, None))
    )
    val updates2 = context.receiveNIgnoreExpressionUpdates(3)
    updates2.length shouldEqual 3
    updates2 should contain allOf (
      Api.Response(requestId, Api.RecomputeContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    val indexedModules2 = updates1.collect {
      case Api.Response(
            None,
            Api.SuggestionsDatabaseModuleUpdateNotification(moduleName, _, _, _)
          ) =>
        moduleName
    }
    indexedModules2 should contain theSameElementsAs Seq(moduleName)
    context.consumeOut shouldEqual List("Hello World!")
  }
}
