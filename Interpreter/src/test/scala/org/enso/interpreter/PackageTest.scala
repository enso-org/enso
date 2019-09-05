package org.enso.interpreter

import java.io.File

import org.enso.interpreter.runtime.RuntimeOptions
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.Source
import org.graalvm.polyglot.Value
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.enso.pkg.Package

trait PackageTest extends FlatSpec with Matchers with ValueEquality {

  def evalTestProject(name: String): Value = {
    val pkgPath =
      new File(getClass.getClassLoader.getResource(name).getPath)
    val mainFile = Package.fromDirectory(pkgPath).get.mainFile
    val context = Context
      .newBuilder(Constants.LANGUAGE_ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.getPackagesPathOption, pkgPath.getAbsolutePath)
      .out(System.out)
      .in(System.in)
      .build()
    context.eval(Source.newBuilder(Constants.LANGUAGE_ID, mainFile).build)
  }
}
