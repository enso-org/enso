package org.enso.interpreter.test.instrument

import org.enso.distribution.FileSystem
import org.enso.distribution.locking.ThreadSafeFileLockManager
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.pkg.{Package, PackageManager}
import org.enso.polyglot._
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.enso.text.{ContentVersion, Sha3_224VersionCalculator}
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeSuggestionUpdatesTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: TestContext = _

  class TestContext(packageName: String) extends InstrumentTestContext {

    val tmpDir: Path = Files.createTempDirectory("enso-test-packages")
    sys.addShutdownHook(FileSystem.removeDirectoryIfExists(tmpDir))
    val lockManager = new ThreadSafeFileLockManager(tmpDir.resolve("locks"))
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
        .option(RuntimeOptions.LOG_LEVEL, "WARNING")
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

  def contentsVersion(content: String): ContentVersion =
    Sha3_224VersionCalculator.evalVersion(content)

  override protected def beforeEach(): Unit = {
    context = new TestContext("Test")
    val Some(Api.Response(_, Api.InitializedNotification())) = context.receive
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
    val version  = contentsVersion(code)
    val mainFile = context.writeMain(code)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, code))
    )
    context.receiveNone shouldEqual None

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
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          version = version,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """from Standard.Base import all
              |
              |main =
              |    x = 42
              |    IO.println x
              |""".stripMargin.linesIterator.mkString("\n")
          ),
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
                        )
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """from Standard.Base import all
              |
              |main =
              |    x = 42
              |    y = 9
              |    IO.println x+y
              |""".stripMargin.linesIterator.mkString("\n")
          ),
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
                        )
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
                        )
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """from Standard.Base import all
              |
              |main =
              |    x = 42
              |    y : Number
              |    y = 9
              |    IO.println x+y
              |""".stripMargin.linesIterator.mkString("\n")
          ),
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
                        )
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
                        )
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """from Standard.Base import all
              |
              |foo x = x * 10
              |
              |main =
              |    x = 42
              |    y : Number
              |    y = 9
              |    IO.println x+y
              |""".stripMargin.linesIterator.mkString("\n")
          ),
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
                        )
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
                        )
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
                  Suggestion.Method(
                    None,
                    moduleName,
                    "foo",
                    List(
                      Suggestion
                        .Argument(
                          "self",
                          "Enso_Test.Test.Main",
                          false,
                          false,
                          None
                        ),
                      Suggestion
                        .Argument("x", ConstantsGen.ANY, false, false, None)
                    ),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """from Standard.Base import all
              |
              |foo a b = a * b
              |
              |main =
              |    x = 42
              |    y : Number
              |    y = 9
              |    IO.println x+y
              |""".stripMargin.linesIterator.mkString("\n")
          ),
          actions = Vector(),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "foo",
                    List(
                      Suggestion
                        .Argument(
                          "self",
                          "Enso_Test.Test.Main",
                          false,
                          false,
                          None
                        ),
                      Suggestion
                        .Argument("x", ConstantsGen.ANY, false, false, None)
                    ),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Modify(
                    None,
                    Some(
                      List(
                        Api.SuggestionArgumentAction
                          .Modify(1, Some("a"), None, None, None, None),
                        Api.SuggestionArgumentAction.Add(
                          2,
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
    val version  = contentsVersion(contents)
    val mainFile = context.writeMain(contents)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open file
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, contents))
    )
    context.receiveNone shouldEqual None

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
    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = moduleName,
          version = version,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module(
                    moduleName,
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    Seq(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
                          Suggestion.Position(9, 0)
                        )
                      ),
                      Api.SuggestionAction.Add()
                    ),
                    Vector()
                  )
                )
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
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
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
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
                    None,
                    None,
                    None
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
      """import Standard.Base.IO
        |
        |import Enso_Test.Test.A
        |from Enso_Test.Test.A export all
        |import Enso_Test.Test.A.MyType
        |from Enso_Test.Test.A.MyType export all
        |
        |main = IO.println "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")
    val aCode =
      """from Standard.Base.Data.Numbers import Integer
        |
        |type MyType
        |    MkA a
        |
        |Integer.fortytwo self = 42
        |
        |hello = "Hello World!"
        |""".stripMargin.linesIterator.mkString("\n")

    val mainVersion = contentsVersion(mainCode)
    val mainFile    = context.writeMain(mainCode)
    val aVersion    = contentsVersion(aCode)
    val aFile       = context.writeInSrcDir("A", aCode)

    // create context
    context.send(Api.Request(requestId, Api.CreateContextRequest(contextId)))
    context.receive shouldEqual Some(
      Api.Response(requestId, Api.CreateContextResponse(contextId))
    )

    // open files
    context.send(
      Api.Request(Api.OpenFileNotification(mainFile, mainCode))
    )
    context.receiveNone shouldEqual None
    context.send(
      Api.Request(Api.OpenFileNotification(aFile, aCode))
    )
    context.receiveNone shouldEqual None

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
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module  = "Enso_Test.Test.A",
          version = aVersion,
          actions =
            Vector(Api.SuggestionsDatabaseAction.Clean("Enso_Test.Test.A")),
          exports = Vector(),
          updates = Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Module("Enso_Test.Test.A", None, None, None),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Atom(
                    None,
                    "Enso_Test.Test.A",
                    "MkA",
                    List(
                      Suggestion
                        .Argument("a", ConstantsGen.ANY, false, false, None)
                    ),
                    "Enso_Test.Test.A.MyType",
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
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
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
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
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    "Enso_Test.Test.A",
                    "hello",
                    List(
                      Suggestion.Argument(
                        "self",
                        "Enso_Test.Test.A",
                        false,
                        false,
                        None
                      )
                    ),
                    "Enso_Test.Test.A",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
          version = mainVersion,
          actions = Vector(Api.SuggestionsDatabaseAction.Clean(moduleName)),
          exports = Vector(
            Api.ExportsUpdate(
              ModuleExports(
                "Enso_Test.Test.Main",
                Set(
                  ExportedSymbol.Atom("Enso_Test.Test.A", "MkA"),
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
                    None,
                    None,
                    None
                  ),
                  Api.SuggestionAction.Add()
                ),
                Vector()
              ),
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestion.Method(
                    None,
                    moduleName,
                    "main",
                    List(),
                    "Enso_Test.Test.Main",
                    ConstantsGen.ANY,
                    None,
                    None,
                    None
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
              model.Range(model.Position(3, 32), model.Position(3, 32)),
              " hiding hello"
            )
          ),
          execute = true
        )
      )
    )
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """import Standard.Base.IO
              |
              |import Enso_Test.Test.A
              |from Enso_Test.Test.A export all hiding hello
              |import Enso_Test.Test.A.MyType
              |from Enso_Test.Test.A.MyType export all
              |
              |main = IO.println "Hello World!"
              |""".stripMargin.linesIterator.mkString("\n")
          ),
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
    context.receiveN(2) should contain theSameElementsAs Seq(
      Api.Response(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          module = moduleName,
          version = contentsVersion(
            """import Standard.Base.IO
              |
              |main = IO.println "Hello World!"
              |""".stripMargin.linesIterator.mkString("\n")
          ),
          actions = Vector(),
          exports = Vector(
            Api.ExportsUpdate(
              ModuleExports(
                "Enso_Test.Test.Main",
                Set(ExportedSymbol.Atom("Enso_Test.Test.A", "MkA"))
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

}
