package org.enso.polyglot
import java.io.File
import java.nio.file.Files

import org.graalvm.polyglot.{Context, PolyglotException}
import org.scalatest.{FlatSpec, Matchers}
import org.enso.pkg.{Package, QualifiedName}

import scala.util.Try

class ModuleManagementTest extends FlatSpec with Matchers {
  class TestContext(packageName: String) {
    val tmpDir: File = Files.createTempDirectory("enso-test-packages").toFile
    val pkg: Package = Package.create(tmpDir, packageName)
    val executionContext = new ExecutionContext(
      Context
        .newBuilder(LanguageInfo.ID)
        .allowExperimentalOptions(true)
        .allowAllAccess(true)
        .option(RuntimeOptions.getPackagesPathOption, pkg.root.getAbsolutePath)
        .build()
    )

    def mkFile(name: String): File = new File(tmpDir, name)

    def writeFile(name: String, contents: String): Unit = {
      Files.write(mkFile(name).toPath, contents.getBytes)
    }

    def writeMain(contents: String): Unit = {
      Files.write(pkg.mainFile.toPath, contents.getBytes)
    }
  }

  val subject = "Modules Polyglot API"

  subject should "allow to trigger reparsing of on-disk sources" in {
    val ctx = new TestContext("Test")

    ctx.writeMain("""
                    |main =
                    |    12345
                    |""".stripMargin)

    val mainModule = ctx.executionContext.getTopScope.getModule("Test.Main")
    val assocCons  = mainModule.getAssociatedConstructor
    val mainFun1   = mainModule.getMethod(assocCons, "main")

    mainFun1.execute(assocCons).asLong() shouldEqual 12345L

    ctx.writeMain("""
                    |main = 4567
                    |""".stripMargin)

    mainModule.reparse()
    val mainFun2 = mainModule.getMethod(assocCons, "main")
    mainFun2.execute(assocCons).asLong() shouldEqual 4567L
  }

  subject should "allow to switch back and forth between literal and on-disk sources" in {
    val ctx = new TestContext("Test")

    ctx.writeMain("""
                    |main = 123
                    |""".stripMargin)

    val mainModule = ctx.executionContext.getTopScope.getModule("Test.Main")
    val assocCons  = mainModule.getAssociatedConstructor
    val mainFun1   = mainModule.getMethod(assocCons, "main")

    mainFun1.execute(assocCons).asLong() shouldEqual 123L

    mainModule.setSource("""
                           |main = 456
                           |""".stripMargin)
    val mainFun2 = mainModule.getMethod(assocCons, "main")
    mainFun2.execute(assocCons).asLong() shouldEqual 456L

    mainModule.setSource("""
                           |main = 789
                           |""".stripMargin)
    val mainFun3 = mainModule.getMethod(assocCons, "main")
    mainFun3.execute(assocCons).asLong() shouldEqual 789L

    ctx.writeMain("""
                    |main = 987
                    |""".stripMargin)

    mainModule.setSourceFile(ctx.pkg.mainFile.getAbsolutePath)
    val mainFun4 = mainModule.getMethod(assocCons, "main")
    mainFun4.execute(assocCons).asLong() shouldEqual 987L
  }

  subject should "allow to create new, importable modules" in {
    val ctx = new TestContext("Test")
    ctx.writeMain("""
                    |import MyLib.Foo
                    |
                    |main = Foo.foo + 1
                    |""".stripMargin)

    ctx.writeFile("Foo.enso", """
                                |foo = 10
                                |""".stripMargin)

    val topScope = ctx.executionContext.getTopScope
    topScope.registerModule("MyLib.Foo", ctx.mkFile("Foo.enso").getAbsolutePath)

    val mainModule = topScope.getModule("Test.Main")
    val assocCons  = mainModule.getAssociatedConstructor
    val mainFun    = mainModule.getMethod(assocCons, "main")
    mainFun.execute(assocCons).asLong shouldEqual 11L
  }

  subject should "allow importing literal-source modules" in {
    val ctx = new TestContext("Test")
    ctx.writeMain("""
                    |import MyLib.Foo
                    |
                    |main = Foo.foo + 1
                    |""".stripMargin)

    ctx.writeFile("Foo.enso", """
                                |foo = 10
                                |""".stripMargin)

    val topScope = ctx.executionContext.getTopScope
    val fooModule = topScope.registerModule(
      "MyLib.Foo",
      ctx.mkFile("Foo.enso").getAbsolutePath
    )
    fooModule.setSource("""
                          |foo = 20
                          |""".stripMargin)

    val mainModule = topScope.getModule("Test.Main")
    val assocCons  = mainModule.getAssociatedConstructor
    val mainFun    = mainModule.getMethod(assocCons, "main")
    mainFun.execute(assocCons).asLong shouldEqual 21L
  }

  subject should "allow for module deletions" in {
    val ctx = new TestContext("Test")

    ctx.writeMain("""
                    |foo = 123
                    |""".stripMargin)

    val mod1 = ctx.executionContext.evalModule(
      """
        |import Test.Main
        |
        |bar = Main.foo + 1
        |""".stripMargin,
      "X"
    )
    val mod1AssocCons = mod1.getAssociatedConstructor
    val mod1Main      = mod1.getMethod(mod1AssocCons, "bar")
    mod1Main.execute(mod1AssocCons).asLong shouldEqual 124

    ctx.executionContext.getTopScope.unregisterModule("Test.Main")

    val mod2 = ctx.executionContext.evalModule(
      """
        |import Test.Main
        |
        |bar = Main.foo + 1
        |""".stripMargin,
      "X2"
    )
    val exception = the[PolyglotException] thrownBy mod2.getAssociatedConstructor
    exception.getMessage shouldEqual "Module Test.Main does not exist."
  }
}
