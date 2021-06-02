package org.enso.docsrunner

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

  // languageContext.getCompiler.generateDocs()
}
