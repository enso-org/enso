package org.enso.polyglot

import java.io.File
import java.nio.file.Files

import org.enso.pkg.{Package, PackageManager}
import org.graalvm.polyglot.{Context, PolyglotException}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleManagementTest extends AnyFlatSpec with Matchers {
  class TestContext(packageName: String) {
    val tmpDir: File = Files.createTempDirectory("enso-test-packages").toFile
    val pkg: Package[File] =
      PackageManager.Default.create(tmpDir, packageName, "Enso_Test")
    val executionContext = new PolyglotContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getAbsolutePath)
        .option(RuntimeOptions.STRICT_ERRORS, "true")
        .build()
    )

    def mkFile(name: String): File = new File(tmpDir, name)

    def writeFile(name: String, contents: String): Unit = {
      Files.write(mkFile(name).toPath, contents.getBytes): Unit
    }

    def writeMain(contents: String): Unit = {
      Files.write(pkg.mainFile.toPath, contents.getBytes): Unit
    }
  }

  val subject = "Modules Polyglot API"

  subject should "allow to trigger reparsing of on-disk sources" in {
    val ctx = new TestContext("Test")

    ctx.writeMain("""
                    |main =
                    |    12345
                    |""".stripMargin)

    val mainModule =
      ctx.executionContext.getTopScope.getModule("Enso_Test.Test.Main")
    val assocCons = mainModule.getAssociatedConstructor
    val mainFun1  = mainModule.getMethod(assocCons, "main").get

    mainFun1.execute(assocCons).asLong() shouldEqual 12345L

    ctx.writeMain("""
                    |main = 4567
                    |""".stripMargin)

    mainModule.reparse()
    val mainFun2 = mainModule.getMethod(assocCons, "main").get
    mainFun2.execute(assocCons).asLong() shouldEqual 4567L
  }

  subject should "allow to switch back and forth between literal and on-disk sources" in {
    val ctx = new TestContext("Test")

    ctx.writeMain("""
                    |main = 123
                    |""".stripMargin)

    val mainModule =
      ctx.executionContext.getTopScope.getModule("Enso_Test.Test.Main")
    val assocCons = mainModule.getAssociatedConstructor
    val mainFun1  = mainModule.getMethod(assocCons, "main").get

    mainFun1.execute(assocCons).asLong() shouldEqual 123L

    mainModule.setSource("""
                           |main = 456
                           |""".stripMargin)
    val mainFun2 = mainModule.getMethod(assocCons, "main").get
    mainFun2.execute(assocCons).asLong() shouldEqual 456L

    mainModule.setSource("""
                           |main = 789
                           |""".stripMargin)
    val mainFun3 = mainModule.getMethod(assocCons, "main").get
    mainFun3.execute(assocCons).asLong() shouldEqual 789L

    ctx.writeMain("""
                    |main = 987
                    |""".stripMargin)

    mainModule.setSourceFile(ctx.pkg.mainFile.getAbsolutePath)
    val mainFun4 = mainModule.getMethod(assocCons, "main").get
    mainFun4.execute(assocCons).asLong() shouldEqual 987L
  }

  subject should "allow to create new, importable modules" in {
    val ctx = new TestContext("Test")
    ctx.writeMain("""
                    |import Enso_Test.Test.Foo
                    |
                    |main = Foo.foo + 1
                    |""".stripMargin)

    ctx.writeFile(
      "Foo.enso",
      """
        |foo = 10
        |""".stripMargin
    )

    val topScope = ctx.executionContext.getTopScope
    topScope.registerModule(
      "Enso_Test.Test.Foo",
      ctx.mkFile("Foo.enso").getAbsolutePath
    )

    val mainModule = topScope.getModule("Enso_Test.Test.Main")
    val assocCons  = mainModule.getAssociatedConstructor
    val mainFun    = mainModule.getMethod(assocCons, "main").get
    mainFun.execute(assocCons).asLong shouldEqual 11L
  }

  subject should "allow importing literal-source modules" in {
    val ctx = new TestContext("Test")
    ctx.writeMain("""
                    |import Enso_Test.Test.Foo
                    |
                    |main = Foo.foo + 1
                    |""".stripMargin)

    ctx.writeFile(
      "Foo.enso",
      """
        |foo = 10
        |""".stripMargin
    )

    val topScope = ctx.executionContext.getTopScope
    val fooModule = topScope.registerModule(
      "Enso_Test.Test.Foo",
      ctx.mkFile("Foo.enso").getAbsolutePath
    )
    fooModule.setSource("""
                          |foo = 20
                          |""".stripMargin)

    val mainModule = topScope.getModule("Enso_Test.Test.Main")
    val assocCons  = mainModule.getAssociatedConstructor
    val mainFun    = mainModule.getMethod(assocCons, "main").get
    mainFun.execute(assocCons).asLong shouldEqual 21L
  }

  subject should "allow for module deletions" in {
    val ctx = new TestContext("Test")

    ctx.writeMain("""
                    |foo = 123
                    |""".stripMargin)

    val mod1 = ctx.executionContext.evalModule(
      """
        |import Enso_Test.Test.Main
        |
        |bar = Main.foo + 1
        |""".stripMargin,
      "X"
    )
    val mod1AssocCons = mod1.getAssociatedConstructor
    val mod1Main      = mod1.getMethod(mod1AssocCons, "bar").get
    mod1Main.execute(mod1AssocCons).asLong shouldEqual 124

    ctx.executionContext.getTopScope.unregisterModule("Enso_Test.Test.Main")

    val mod2 = ctx.executionContext.evalModule(
      """
        |import Enso_Test.Test.Main
        |
        |bar = Main.foo + 1
        |""".stripMargin.replace("\r\n", "\n"),
      "X2"
    )
    val exception =
      the[PolyglotException] thrownBy mod2.getAssociatedConstructor
    exception.getMessage shouldEqual "Compilation aborted due to errors."
  }
}
