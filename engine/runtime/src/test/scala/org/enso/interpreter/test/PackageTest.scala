package org.enso.interpreter.test

import java.io.File

import org.enso.pkg.Package
import org.enso.polyglot.{LanguageInfo, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait PackageTest extends AnyFlatSpec with Matchers with ValueEquality {

  def evalTestProject(name: String): Value = {
    val pkgPath =
      new File(getClass.getClassLoader.getResource(name).getPath)
    val pkg        = Package.fromDirectory(pkgPath).get
    val mainFile   = pkg.mainFile
    val mainModule = pkg.moduleNameForFile(mainFile)
    val context = Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.getPackagesPathOption, pkgPath.getAbsolutePath)
      .out(System.out)
      .in(System.in)
      .build()
    context.initialize(LanguageInfo.ID)
    val executionContext = new PolyglotContext(context)
    val topScope         = executionContext.getTopScope
    val mainModuleScope  = topScope.getModule(mainModule.toString)
    val assocCons        = mainModuleScope.getAssociatedConstructor
    val mainFun          = mainModuleScope.getMethod(assocCons, "main")
    InterpreterException.rethrowPolyglot(mainFun.execute(assocCons))
  }
}
