package org.enso.runner

import java.io.InputStream
import java.io.OutputStream

import org.enso.loggingservice.{JavaLoggingLogHandler, LogLevel}
import org.enso.polyglot.debugger.{
  DebugServerInfo,
  DebuggerSessionManagerEndpoint
}
import org.enso.polyglot.{LanguageInfo, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.Context

/** Utility class for creating Graal polyglot contexts.
  */
class ContextFactory {

  /** Creates a new Graal polyglot context.
    *
    * @param packagesPath Enso packages path
    * @param in the input stream for standard in
    * @param out the output stream for standard out
    * @param repl the Repl manager to use for this context
    * @param logLevel the log level for this context
    * @param strictErrors whether or not to use strict errors
    * @return configured Context instance
    */
  def create(
    packagesPath: String = "",
    in: InputStream,
    out: OutputStream,
    repl: Repl,
    logLevel: LogLevel,
    strictErrors: Boolean = false
  ): PolyglotContext = {
    val context = Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.PACKAGES_PATH, packagesPath)
      .option(RuntimeOptions.STRICT_ERRORS, strictErrors.toString)
      .option(DebugServerInfo.ENABLE_OPTION, "true")
      .out(out)
      .in(in)
      .serverTransport { (uri, peer) =>
        if (uri.toString == DebugServerInfo.URI) {
          new DebuggerSessionManagerEndpoint(repl, peer)
        } else null
      }
      .option(
        RuntimeOptions.LOG_LEVEL,
        JavaLoggingLogHandler.getJavaLogLevelFor(logLevel).getName
      )
      .logHandler(
        JavaLoggingLogHandler.create(JavaLoggingLogHandler.defaultLevelMapping)
      )
      .build
    new PolyglotContext(context)
  }
}
