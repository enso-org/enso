package org.enso.docsrunner

import java.io.File
import org.enso.interpreter.runtime.{Context => EnsoContext}
import org.enso.languageserver.io.{
  ObservableOutputStream,
  ObservablePipedInputStream
}
import org.enso.polyglot.{LanguageInfo, MethodNames}
import org.graalvm.polyglot.Context

object Main {
  val stdOut    = new ObservableOutputStream
  val stdErr    = new ObservableOutputStream
  val stdInSink = new ObservableOutputStream
  val stdIn     = new ObservablePipedInputStream(stdInSink)

  def generateFrom(file: File): Unit = {
    val executionContext = Context
      .newBuilder()
      .allowAllAccess(true)
      .allowExperimentalOptions(true)
      .out(stdOut)
      .err(stdErr)
      .in(stdIn)
      .build()

    val languageContext = executionContext
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[EnsoContext]

    val module = languageContext.getModuleForFile(file)

    val generated = module.map(languageContext.getCompiler.generateDocs)
    // TODO:
    // - go through executed code and get all HTML docs
    //   with their corresponding atoms/methods etc.
    // - Save those to files
    print(generated)
  }

  def main(args: Array[String]): Unit = {
    // Go through files executing generateFrom(File f)
    val stdLibPath   = "./distribution/std-lib/Standard/src"
    val tempFileName = "Base/Geo.enso"
    val file         = new File(stdLibPath + "/" + tempFileName)
    generateFrom(file)
  }
}
