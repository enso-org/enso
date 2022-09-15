package org.enso.compiler.test

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.{IOFileFilter, TrueFileFilter}
import org.enso.interpreter.test.{InterpreterException, ValueEquality}
import org.enso.pkg.PackageManager
import org.enso.polyglot.{LanguageInfo, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File, FileFilter}
import java.nio.file.{Files, Paths, StandardCopyOption}

trait ModifiedTest
    extends AnyFlatSpec
    with Matchers
    with ValueEquality
    with BeforeAndAfterAll {
  val output                            = new ByteArrayOutputStream()
  private[this] var testDirectory: File = _

  /** Compile and run a project defined in the resources directory.
    * The project may contain files that are changed in the process.
    * The changes to files are specified via iteration suffix. E.g.,
    * A_1.enso, A_2.enso and A_3.enso represents three states of the
    * A.enso file.
    *
    * @param name name of the project to run
    * @param iteration iteration used to identify the correct version of the module
    * @return result of executing the project
    */
  def evalTestProjectIteration(name: String, iteration: Int): Value = {
    assert(iteration > 0)

    val pkgPath =
      new File(getClass.getClassLoader.getResource(name).getPath)

    val testPkgPath = testDirectory.toPath.resolve(name).toFile
    if (!testPkgPath.exists()) {
      initialCopy(pkgPath, testPkgPath)
    }
    copyIterationFiles(pkgPath, testPkgPath, iteration)
    val pkg        = PackageManager.Default.fromDirectory(testPkgPath).get
    val mainFile   = pkg.mainFile
    val mainModule = pkg.moduleNameForFile(mainFile)
    val context = Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.PROJECT_ROOT, testPkgPath.getAbsolutePath)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths
          .get("../../test/micro-distribution/component")
          .toFile
          .getAbsolutePath
      )
      .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
      .option(RuntimeOptions.STRICT_ERRORS, "true")
      .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
      .out(output)
      .in(System.in)
      .option(RuntimeOptions.LOG_LEVEL, "WARNING")
      .logHandler(System.err)
      .build()
    context.initialize(LanguageInfo.ID)
    val executionContext = new PolyglotContext(context)
    InterpreterException.rethrowPolyglot {
      val topScope        = executionContext.getTopScope
      val mainModuleScope = topScope.getModule(mainModule.toString)
      val assocCons       = mainModuleScope.getAssociatedType
      val mainFun         = mainModuleScope.getMethod(assocCons, "main").get
      mainFun.execute()
    }
  }

  private def initialCopy(from: File, to: File): Unit = {
    val filter = new FileFilter {
      override def accept(pathname: File): Boolean = {
        !pathname.getName.matches(".+_(\\d+)\\.enso")
      }
    }
    copySources(from, to, filter)
  }

  private def copyIterationFiles(from: File, to: File, iteration: Int): Unit = {
    val iterationFileFilter = new IOFileFilter {
      override def accept(pathname: File): Boolean = {
        pathname.getName.matches(s".+_$iteration\\.enso")
      }

      override def accept(dir: File, name: String): Boolean = {
        name.matches(s".+_$iteration\\.enso")
      }
    }
    val allIterationFiles =
      FileUtils.listFiles(from, iterationFileFilter, TrueFileFilter.INSTANCE)
    allIterationFiles.forEach(f => {
      val path          = from.toPath.relativize(f.toPath)
      val fixedFileName = path.toString.replaceFirst("_\\d+\\.enso", ".enso")
      val targetFile    = to.toPath.resolve(fixedFileName)
      FileUtils.copyFile(
        f,
        targetFile.toFile,
        StandardCopyOption.REPLACE_EXISTING
      )
    })
  }

  private def copySources(
    from: File,
    to: File,
    fileFilter: FileFilter
  ): Unit = {
    to.mkdir()
    FileUtils.copyDirectory(from, to, fileFilter)
  }

  override def beforeAll(): Unit = {
    testDirectory = Files.createTempDirectory("enso-projects").toFile
  }

  override def afterAll(): Unit = {
    FileUtils.forceDelete(testDirectory)
  }

  def consumeOut: List[String] = {
    val result = output.toString
    output.reset()
    result.linesIterator.toList
  }
}
