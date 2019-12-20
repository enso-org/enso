package org.enso.interpreter.test

import java.io.File

import org.enso.interpreter.Constants
import org.enso.interpreter.runtime.RuntimeOptions
import org.enso.pkg.Package
import org.graalvm.polyglot.{Context, Source, Value}
import org.scalatest.{FlatSpec, Matchers}

trait PackageTest extends FlatSpec with Matchers with ValueEquality {

  def evalTestProject(name: String): Value = {
    val pkgPath =
      new File(getClass.getClassLoader.getResource(name).getPath)
    val pkg        = Package.fromDirectory(pkgPath).get
    val mainFile   = pkg.mainFile
    val mainModule = pkg.moduleNameForFile(mainFile)
    val context = Context
      .newBuilder(Constants.LANGUAGE_ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.getPackagesPathOption, pkgPath.getAbsolutePath)
      .out(System.out)
      .in(System.in)
      .build()
    context.initialize(Constants.LANGUAGE_ID)
    val bindings        = context.getBindings(Constants.LANGUAGE_ID)
    val mainModuleScope = bindings.invokeMember("get_module", mainModule.toString)
    val assocCons       = mainModuleScope.invokeMember("get_associated_constructor")
    val mainFun         = mainModuleScope.invokeMember("get_method", assocCons, "main")
    InterpreterException.rethrowPolyglot(mainFun.execute(assocCons))
  }
}
