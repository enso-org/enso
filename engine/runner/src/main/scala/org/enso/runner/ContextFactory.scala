package org.enso.runner

import org.enso.loggingservice.{JavaLoggingLogHandler, LogLevel}
import org.enso.polyglot.debugger.{
  DebugServerInfo,
  DebuggerSessionManagerEndpoint
}
import org.enso.polyglot.{HostAccessFactory, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.Context

import java.io.{InputStream, OutputStream}

/** Utility class for creating Graal polyglot contexts.
  */
class ContextFactory {

  /** Creates a new Graal polyglot context.
    *
    * @param projectRoot root of the project the interpreter is being run in
    *                    (or empty if ran outside of any projects)
    * @param in the input stream for standard in
    * @param out the output stream for standard out
    * @param repl the Repl manager to use for this context
    * @param logLevel the log level for this context
    * @param enableIrCaches whether or not IR caching should be enabled
    * @param strictErrors whether or not to use strict errors
    * @param useGlobalIrCacheLocation whether or not to use the global IR cache
    *                                 location
    * @param options additional options for the Context
    * @param executionEnvironment optional name of the execution environment to use during execution
    * @return configured Context instance
    */
  def create(
    projectRoot: String = "",
    in: InputStream,
    out: OutputStream,
    repl: Repl,
    logLevel: LogLevel,
    logMasking: Boolean,
    enableIrCaches: Boolean,
    strictErrors: Boolean                  = false,
    useGlobalIrCacheLocation: Boolean      = true,
    enableAutoParallelism: Boolean         = false,
    executionEnvironment: Option[String]   = None,
    options: java.util.Map[String, String] = java.util.Collections.emptyMap
  ): PolyglotContext = {
    executionEnvironment.foreach { name =>
      options.put("enso.ExecutionEnvironment", name)
    }
    val context = Context
      .newBuilder()
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .allowHostAccess(new HostAccessFactory().allWithTypeMapping())
      .option(RuntimeOptions.PROJECT_ROOT, projectRoot)
      .option(RuntimeOptions.STRICT_ERRORS, strictErrors.toString)
      .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true")
      .option(
        RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION,
        useGlobalIrCacheLocation.toString
      )
      .option(RuntimeOptions.DISABLE_IR_CACHES, (!enableIrCaches).toString)
      .option(DebugServerInfo.ENABLE_OPTION, "true")
      .option(RuntimeOptions.LOG_MASKING, logMasking.toString)
      .options(options)
      .option(
        RuntimeOptions.ENABLE_AUTO_PARALLELISM,
        enableAutoParallelism.toString
      )
      .option("js.foreign-object-prototype", "true")
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
