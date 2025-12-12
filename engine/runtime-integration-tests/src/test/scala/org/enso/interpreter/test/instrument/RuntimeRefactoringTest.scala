package org.enso.interpreter.test.instrument

import org.apache.commons.io.output.TeeOutputStream
import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.interpreter.test.Metadata
import org.enso.common.LanguageInfo
import org.enso.common.RuntimeOptions
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model
import org.enso.text.editing.model.TextEdit
import org.enso.text.{ContentBasedVersioning, Sha3_224VersionCalculator}
import org.graalvm.polyglot.Context
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.logging.Level

@scala.annotation.nowarn("msg=multiarg infix syntax")
class RuntimeRefactoringTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  // === Test Utilities =======================================================

  var context: TestContext = _

  class TestContext(packageName: String)
      extends InstrumentTestContext(packageName) {

    val out: ByteArrayOutputStream    = new ByteArrayOutputStream()
    val logOut: ByteArrayOutputStream = new ByteArrayOutputStream()
    val context =
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
        .option(RuntimeOptions.CHECK_CWD, "false")
        .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
        .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_PROGRESS_REPORT, "false")
        .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
        .option(RuntimeOptions.ENABLE_EXECUTION_TIMER, "false")
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

    def readMain: String =
      Files.readString(pkg.mainFile.toPath)

    def consumeOut: List[String] = {
      val result = out.toString
      out.reset()
      result.linesIterator.toList
    }

  }

  val versionCalculator: ContentBasedVersioning = Sha3_224VersionCalculator

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

  "RuntimeServer" should "rename operator in main body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idOperator1 = metadata.addItem(42, 9)
    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "foobarbaz"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(3, 4), model.Position(3, 13)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(4, 16), model.Position(4, 25)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("operator1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idOperator1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idOperator1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "rename operator in lambda expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idOperator1 = metadata.addItem(42, 9)
    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = 41
        |    operator2 = x-> operator1 + x
        |    IO.println (operator2 1)
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "foobarbaz"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(3, 4), model.Position(3, 13)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(4, 20), model.Position(4, 29)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("operator1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idOperator1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idOperator1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "edit file after renaming local" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata        = new Metadata
    val symbolOperator1 = metadata.addItem(42, 9, "aa")
    val exprOperator1   = metadata.addItem(54, 2, "ab")
    val exprOperator2   = metadata.addItem(73, 13, "ac")
    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, exprOperator1, ConstantsGen.INTEGER),
      TestMessages.update(
        contextId,
        exprOperator2,
        ConstantsGen.INTEGER,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Data.Numbers",
            ConstantsGen.INTEGER,
            "+"
          )
        )
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "foobarbaz"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(3, 4), model.Position(3, 13)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(4, 16), model.Position(4, 25)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("operator1", newName))
        .toHexString
    )
    context.send(
      Api.Request(
        requestId,
        Api.RenameSymbol(moduleName, symbolOperator1, newName)
      )
    )
    context.receiveNIgnoreStdLib(6) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages
        .pending(contextId, symbolOperator1, exprOperator2, exprOperator1),
      TestMessages.update(
        contextId,
        exprOperator1,
        ConstantsGen.INTEGER,
        typeChanged = false
      ),
      TestMessages.update(
        contextId,
        exprOperator2,
        ConstantsGen.INTEGER,
        Api.MethodCall(
          Api.MethodPointer(
            "Standard.Base.Data.Numbers",
            ConstantsGen.INTEGER,
            "+"
          )
        ),
        fromCache   = false,
        typeChanged = false
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // modify main
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(3, 16), model.Position(3, 18)),
              "42"
            )
          ),
          execute = true,
          idMap   = None
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, exprOperator1, exprOperator2),
      Api.Response(None, Api.ExecutionUpdate(contextId, Seq())),
      TestMessages
        .update(
          contextId,
          exprOperator1,
          ConstantsGen.INTEGER,
          typeChanged = false
        ),
      TestMessages
        .update(
          contextId,
          exprOperator2,
          ConstantsGen.INTEGER,
          Api.MethodCall(
            Api.MethodPointer(
              "Standard.Base.Data.Numbers",
              ConstantsGen.INTEGER,
              "+"
            )
          ),
          fromCache   = false,
          typeChanged = false
        ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("43")
  }

  it should "rename qualified module method in main body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idFunction1 = metadata.addItem(31, 9)
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = 41
        |    operator2 = Main.function1 operator1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(6, 21), model.Position(6, 30)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("function1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idFunction1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idFunction1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "rename to original name qualified module method in main body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idFunction1 = metadata.addItem(31, 9)
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = 41
        |    operator2 = Main.function1 operator1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(6, 21), model.Position(6, 30)),
        newName
      )
    )
    val newContents = contents.replaceAll("function1", newName)
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator.evalVersion(newContents).toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idFunction1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idFunction1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1 back
    val originalName = "function1"
    val expectedEdits1 = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        originalName
      ),
      TextEdit(
        model.Range(model.Position(6, 21), model.Position(6, 30)),
        originalName
      )
    )
    val expectedFileEdit1 = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits1,
      versionCalculator.evalVersion(newContents).toHexString,
      versionCalculator.evalVersion(contents).toHexString
    )
    context.send(
      Api.Request(
        requestId,
        Api.RenameSymbol(moduleName, idFunction1, originalName)
      )
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(originalName)),
      Api.Response(None, expectedFileEdit1),
      TestMessages.pending(contextId, idFunction1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "rename qualified module method in lambda expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idFunction1 = metadata.addItem(31, 9)
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = 41
        |    operator2 = x -> Main.function1 x
        |    IO.println (operator2 operator1)
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(6, 26), model.Position(6, 35)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("function1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idFunction1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idFunction1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "rename unqualified module method in main body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idFunction1 = metadata.addItem(31, 9)
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = function1 41
        |    IO.println operator1
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(5, 16), model.Position(5, 25)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("function1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idFunction1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idFunction1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "rename unqualified module method in lambda expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idFunction1 = metadata.addItem(31, 9)
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = 41
        |    operator2 = x -> function1 x
        |    IO.println (operator2 operator1)
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(6, 21), model.Position(6, 30)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("function1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idFunction1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idFunction1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "edit file after renaming module method" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata        = new Metadata
    val symbolFunction1 = metadata.addItem(31, 9, "aa")
    val exprOperator1   = metadata.addItem(75, 2, "ab")
    val exprOperator2   = metadata.addItem(94, 24, "ac")
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = 41
        |    operator2 = Main.function1 operator1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, exprOperator1, ConstantsGen.INTEGER),
      TestMessages.update(
        contextId,
        exprOperator2,
        ConstantsGen.INTEGER,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "function1"))
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(6, 21), model.Position(6, 30)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("function1", newName))
        .toHexString
    )
    context.send(
      Api.Request(
        requestId,
        Api.RenameSymbol(moduleName, symbolFunction1, newName)
      )
    )
    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, symbolFunction1, exprOperator2),
      TestMessages.update(
        contextId,
        exprOperator2,
        ConstantsGen.INTEGER,
        Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "function2"))
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // modify main
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          Seq(
            TextEdit(
              model.Range(model.Position(5, 16), model.Position(5, 18)),
              "42"
            )
          ),
          execute = true,
          idMap   = None
        )
      )
    )
    context.receiveN(5) should contain theSameElementsAs Seq(
      TestMessages.pending(contextId, exprOperator1, exprOperator2),
      Api.Response(None, Api.ExecutionUpdate(contextId, Seq())),
      TestMessages
        .update(
          contextId,
          exprOperator1,
          ConstantsGen.INTEGER,
          typeChanged = false
        ),
      TestMessages
        .update(
          contextId,
          exprOperator2,
          ConstantsGen.INTEGER,
          typeChanged = false,
          methodCall = Some(
            Api.MethodCall(
              Api.MethodPointer(moduleName, moduleName, "function2")
            )
          )
        ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("43")
  }

  it should "fail with ExpressionNotFound when renaming non-existent symbol" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata = new Metadata
    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val randomExpressionId = UUID.randomUUID()
    val newName            = "foobarbaz"
    context.send(
      Api.Request(
        requestId,
        Api.RenameSymbol(moduleName, randomExpressionId, newName)
      )
    )
    context.receiveNIgnoreStdLib(1) should contain theSameElementsAs Seq(
      Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.ExpressionNotFound(randomExpressionId)
        )
      )
    )
    context.consumeOut shouldEqual List()
  }

  it should "fail with OperationNotSupported when renaming an expression" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata      = new Metadata
    val exprOperator1 = metadata.addItem(54, 2)
    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(3) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, exprOperator1, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "foobarbaz"
    context.send(
      Api.Request(
        requestId,
        Api.RenameSymbol(moduleName, exprOperator1, newName)
      )
    )
    context.receiveNIgnoreStdLib(1) should contain theSameElementsAs Seq(
      Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.OperationNotSupported(exprOperator1)
        )
      )
    )
    context.consumeOut shouldEqual List()
  }

  it should "fail to rename module method when module definition exists" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idFunction1 = metadata.addItem(31, 9)
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |function2 = Nothing
        |
        |main =
        |    operator1 = 41
        |    operator2 = x -> Main.function1 x
        |    IO.println (operator2 operator1)
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "function2"
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idFunction1, newName))
    )
    context.receiveNIgnoreStdLib(1) should contain theSameElementsAs Seq(
      Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.DefinitionAlreadyExists(newName)
        )
      )
    )
    context.consumeOut shouldEqual List()
  }

  it should "fail to rename operator when local definition exists" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"

    val metadata    = new Metadata
    val idOperator1 = metadata.addItem(42, 9)
    val code =
      """from Standard.Base import all
        |
        |main =
        |    operator1 = 41
        |    operator2 = operator1 + 1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val newName = "operator2"
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idOperator1, newName))
    )
    context.receiveNIgnoreStdLib(1) should contain theSameElementsAs Seq(
      Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.DefinitionAlreadyExists(newName)
        )
      )
    )
    context.consumeOut shouldEqual List()
  }

  it should "rename operator if the same definition exists in different method body" in {
    val contextId  = UUID.randomUUID()
    val requestId  = UUID.randomUUID()
    val moduleName = "Enso_Test.Test.Main"
    val newName    = "foobarbaz"

    val metadata    = new Metadata
    val idOperator1 = metadata.addItem(42, 9)
    val code =
      s"""from Standard.Base import all
         |
         |main =
         |    operator1 = 41
         |    operator2 = operator1 + 1
         |    IO.println operator2
         |
         |test =
         |    $newName = 42
         |    $newName
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(2) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // rename operator1
    val expectedEdits = Vector(
      TextEdit(
        model.Range(model.Position(3, 4), model.Position(3, 13)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(4, 16), model.Position(4, 25)),
        newName
      )
    )
    val expectedFileEdit = Api.FileEdit(
      context.pkg.mainFile,
      expectedEdits,
      versionCalculator.evalVersion(contents).toHexString,
      versionCalculator
        .evalVersion(contents.replaceAll("operator1", newName))
        .toHexString
    )
    context.send(
      Api.Request(requestId, Api.RenameSymbol(moduleName, idOperator1, newName))
    )
    context.receiveNIgnoreStdLib(4) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.SymbolRenamed(newName)),
      Api.Response(None, expectedFileEdit),
      TestMessages.pending(contextId, idOperator1),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")
  }

  it should "rename module method in main body by applying text edit" in {
    val contextId       = UUID.randomUUID()
    val requestId       = UUID.randomUUID()
    val visualizationId = UUID.randomUUID()
    val moduleName      = "Enso_Test.Test.Main"

    val metadata      = new Metadata
    val operator1Id   = metadata.addItem(75, 2, "aa")
    val operator2Id   = metadata.addItem(94, 24, "ba")
    val selfOperator2 = metadata.addItem(94, 4, "bb")
    val code =
      """from Standard.Base import all
        |
        |function1 x = x + 1
        |
        |main =
        |    operator1 = 41
        |    operator2 = Main.function1 operator1
        |    IO.println operator2
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
            Api.MethodPointer(moduleName, moduleName, "main"),
            None,
            Vector()
          )
        )
      )
    )

    context.receiveNIgnoreStdLib(5) should contain theSameElementsAs Seq(
      Api.Response(requestId, Api.PushContextResponse(contextId)),
      TestMessages.update(contextId, operator1Id, ConstantsGen.INTEGER),
      TestMessages.update(
        contextId,
        operator2Id,
        ConstantsGen.INTEGER,
        methodCall = Some(
          Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "function1"))
        )
      ),
      TestMessages.update(
        contextId,
        selfOperator2,
        moduleName
      ),
      context.executionComplete(contextId)
    )
    context.consumeOut shouldEqual List("42")

    // attach visualization
    context.send(
      Api.Request(
        requestId,
        Api.AttachVisualization(
          visualizationId,
          operator2Id,
          Api.VisualizationConfiguration(
            contextId,
            Api.VisualizationExpression.Text(
              moduleName,
              "x -> x.to_text",
              Vector()
            ),
            moduleName
          )
        )
      )
    )
    val attachVisualizationResponses =
      context.receiveNIgnoreExpressionUpdates(2, timeoutSeconds = 10)
    attachVisualizationResponses should contain(
      Api.Response(requestId, Api.VisualizationAttached())
    )
    val Some(data) = attachVisualizationResponses.collectFirst {
      case Api.Response(
            None,
            Api.VisualizationUpdate(
              Api.VisualizationContext(
                `visualizationId`,
                `contextId`,
                `operator2Id`
              ),
              data
            )
          ) =>
        data
    }
    data.sameElements("42".getBytes) shouldBe true

    // rename function1
    val newName = "function2"
    val edits = Vector(
      TextEdit(
        model.Range(model.Position(2, 0), model.Position(2, 9)),
        newName
      ),
      TextEdit(
        model.Range(model.Position(6, 21), model.Position(6, 30)),
        newName
      )
    )
    // Modify the file
    context.send(
      Api.Request(
        Api.EditFileNotification(
          mainFile,
          edits,
          execute = true,
          idMap   = None
        )
      )
    )
    val afterModificationResponse =
      context.receiveNIgnorePendingExpressionUpdates(
        4,
        timeoutSeconds = 10
      )
    afterModificationResponse should contain allOf (
      context.executionComplete(contextId),
      TestMessages.update(
        contextId,
        operator2Id,
        ConstantsGen.INTEGER,
        methodCall = Some(
          Api.MethodCall(Api.MethodPointer(moduleName, moduleName, "function2"))
        )
      ),
      TestMessages.update(
        contextId,
        selfOperator2,
        moduleName,
        typeChanged = false
      ),
    )
    context.consumeOut shouldEqual List("42")

    val Some(data2) = afterModificationResponse.collectFirst {
      case Api.Response(
            None,
            Api.VisualizationUpdate(
              Api.VisualizationContext(
                `visualizationId`,
                `contextId`,
                `operator2Id`
              ),
              data
            )
          ) =>
        data
    }
    data2.sameElements("42".getBytes) shouldBe true

  }
}
