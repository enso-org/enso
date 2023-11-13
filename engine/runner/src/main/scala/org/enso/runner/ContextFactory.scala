package org.enso.runner

import org.enso.logger.{Converter, JulHandler, LoggerSetup}
import org.enso.polyglot.debugger.{
  DebugServerInfo,
  DebuggerSessionManagerEndpoint
}
import org.enso.polyglot.{
  HostAccessFactory,
  LanguageInfo,
  PolyglotContext,
  RuntimeOptions
}
import org.graalvm.polyglot.Engine
import org.graalvm.polyglot.Context
import org.slf4j.event.Level

import java.io.{File, InputStream, OutputStream}

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
    * @param disablePrivateCheck If `private` keyword should be disabled.
    * @param strictErrors whether or not to use strict errors
    * @param useGlobalIrCacheLocation whether or not to use the global IR cache
    *                                 location
    * @param options additional options for the Context
    * @param executionEnvironment optional name of the execution environment to use during execution
    * @param warningsLimit maximal number of warnings reported to the user
    * @return configured Context instance
    */
  def create(
    projectRoot: String = "",
    in: InputStream,
    out: OutputStream,
    repl: Repl,
    logLevel: Level,
    logMasking: Boolean,
    enableIrCaches: Boolean,
    disablePrivateCheck: Boolean           = false,
    strictErrors: Boolean                  = false,
    useGlobalIrCacheLocation: Boolean      = true,
    enableAutoParallelism: Boolean         = false,
    executionEnvironment: Option[String]   = None,
    warningsLimit: Int                     = 100,
    options: java.util.Map[String, String] = java.util.Collections.emptyMap
  ): PolyglotContext = {
    executionEnvironment.foreach { name =>
      options.put("enso.ExecutionEnvironment", name)
    }
    var javaHome = System.getenv("JAVA_HOME");
    if (javaHome == null) {
      javaHome = System.getProperty("java.home");
    }
    if (javaHome == null) {
      throw new IllegalStateException("Specify JAVA_HOME environment property");
    }
    val julLogLevel  = Converter.toJavaLevel(logLevel)
    val logLevelName = julLogLevel.getName
    val builder = Context
      .newBuilder()
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .allowHostAccess(
        new HostAccessFactory()
          .allWithTypeMapping()
      )
      .option(RuntimeOptions.PROJECT_ROOT, projectRoot)
      .option(RuntimeOptions.STRICT_ERRORS, strictErrors.toString)
      .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true")
      .option(
        RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION,
        useGlobalIrCacheLocation.toString
      )
      .option(RuntimeOptions.DISABLE_IR_CACHES, (!enableIrCaches).toString)
      .option(
        RuntimeOptions.DISABLE_PRIVATE_CHECK,
        disablePrivateCheck.toString
      )
      .option(DebugServerInfo.ENABLE_OPTION, "true")
      .option(RuntimeOptions.LOG_MASKING, logMasking.toString)
      .options(options)
      .option(
        RuntimeOptions.ENABLE_AUTO_PARALLELISM,
        enableAutoParallelism.toString
      )
      .option(
        RuntimeOptions.WARNINGS_LIMIT,
        warningsLimit.toString
      )
      .option("js.foreign-object-prototype", "true")
      .out(out)
      .in(in)
      .serverTransport { (uri, peer) =>
        if (uri.toString == DebugServerInfo.URI) {
          new DebuggerSessionManagerEndpoint(repl, peer)
        } else null
      }

    builder.option(RuntimeOptions.LOG_LEVEL, logLevelName)
    val logHandler = JulHandler.get()
    val logLevels  = LoggerSetup.get().getConfig.getLoggers
    if (logLevels.hasEnsoLoggers()) {
      logLevels.entrySet().forEach { entry =>
        builder.option(
          s"log.${LanguageInfo.ID}.${entry.getKey}.level",
          Converter.toJavaLevel(entry.getValue).getName
        )
      }
    }
    builder
      .logHandler(logHandler)

    val graalpy = new File(
      new File(
        new File(new File(new File(projectRoot), "polyglot"), "python"),
        "bin"
      ),
      "graalpy"
    )
    if (graalpy.exists()) {
      builder.option("python.Executable", graalpy.getAbsolutePath());
    }
    if (
      Engine
        .newBuilder()
        .allowExperimentalOptions(true)
        .build()
        .getLanguages()
        .containsKey("java")
    ) {
      builder
        .option("java.ExposeNativeJavaVM", "true")
        .option("java.Polyglot", "true")
        .option("java.UseBindingsLoader", "true")
        .option("java.JavaHome", javaHome)
        .allowCreateThread(true)
    }
    new PolyglotContext(builder.build)
  }
}
