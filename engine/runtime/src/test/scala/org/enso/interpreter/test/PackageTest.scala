package org.enso.interpreter.test

import java.io.File

import org.enso.pkg.PackageManager
import org.enso.polyglot.{LanguageInfo, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait PackageTest extends AnyFlatSpec with Matchers with ValueEquality {

  def evalTestProject(name: String): Value = {
    val pkgPath =
      new File(getClass.getClassLoader.getResource(name).getPath)
    val pkg        = PackageManager.Default.fromDirectory(pkgPath).get
    val mainFile   = pkg.mainFile
    val mainModule = pkg.moduleNameForFile(mainFile)
    val context = Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.PACKAGES_PATH, pkgPath.getAbsolutePath)
      .option(RuntimeOptions.STRICT_ERRORS, "true")
      .out(System.out)
      .in(System.in)
      .build()
    context.initialize(LanguageInfo.ID)
    val executionContext = new PolyglotContext(context)
    InterpreterException.rethrowPolyglot {
      val topScope        = executionContext.getTopScope
      val mainModuleScope = topScope.getModule(mainModule.toString)
      val assocCons       = mainModuleScope.getAssociatedConstructor
      val mainFun         = mainModuleScope.getMethod(assocCons, "main")
      mainFun.execute(assocCons)
    }
  }
}
