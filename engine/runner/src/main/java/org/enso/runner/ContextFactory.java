package org.enso.runner;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import org.enso.logger.Converter;
import org.enso.logger.JulHandler;
import org.enso.logger.LoggerSetup;
import org.enso.polyglot.HostAccessFactory;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.polyglot.debugger.DebugServerInfo;
import org.enso.polyglot.debugger.DebuggerSessionManagerEndpoint;
import org.graalvm.polyglot.Context;
import org.slf4j.event.Level;

/**
 * Builder to create a new Graal polyglot context.
 *
 * @param projectRoot root of the project the interpreter is being run in (or empty if ran outside
 *     of any projects)
 * @param in the input stream for standard in
 * @param out the output stream for standard out
 * @param repl the Repl manager to use for this context
 * @param logLevel the log level for this context
 * @param enableIrCaches whether or not IR caching should be enabled
 * @param disablePrivateCheck If `private` keyword should be disabled.
 * @param strictErrors whether or not to use strict errors
 * @param useGlobalIrCacheLocation whether or not to use the global IR cache location
 * @param options additional options for the Context
 * @param executionEnvironment optional name of the execution environment to use during execution
 * @param warningsLimit maximal number of warnings reported to the user
 */
final class ContextFactory {
  private String projectRoot;
  private InputStream in;
  private OutputStream out;
  private Repl repl;
  private Level logLevel;
  private boolean logMasking;
  private boolean enableIrCaches;
  private boolean disablePrivateCheck;
  private boolean strictErrors;
  private boolean useGlobalIrCacheLocation = true;
  private boolean enableAutoParallelism;
  private String executionEnvironment;
  private int warningsLimit = 100;
  private java.util.Map<String, String> options = java.util.Collections.emptyMap();

  private ContextFactory() {}

  public static ContextFactory create() {
    return new ContextFactory();
  }

  public ContextFactory projectRoot(String projectRoot) {
    this.projectRoot = projectRoot;
    return this;
  }

  public ContextFactory in(InputStream in) {
    this.in = in;
    return this;
  }

  public ContextFactory out(OutputStream out) {
    this.out = out;
    return this;
  }

  public ContextFactory repl(Repl repl) {
    this.repl = repl;
    return this;
  }

  public ContextFactory logLevel(Level logLevel) {
    this.logLevel = logLevel;
    return this;
  }

  public ContextFactory logMasking(boolean logMasking) {
    this.logMasking = logMasking;
    return this;
  }

  public ContextFactory enableIrCaches(boolean enableIrCaches) {
    this.enableIrCaches = enableIrCaches;
    return this;
  }

  public ContextFactory disablePrivateCheck(boolean disablePrivateCheck) {
    this.disablePrivateCheck = disablePrivateCheck;
    return this;
  }

  public ContextFactory strictErrors(boolean strictErrors) {
    this.strictErrors = strictErrors;
    return this;
  }

  public ContextFactory useGlobalIrCacheLocation(boolean useGlobalIrCacheLocation) {
    this.useGlobalIrCacheLocation = useGlobalIrCacheLocation;
    return this;
  }

  public ContextFactory enableAutoParallelism(boolean enableAutoParallelism) {
    this.enableAutoParallelism = enableAutoParallelism;
    return this;
  }

  public ContextFactory executionEnvironment(String executionEnvironment) {
    this.executionEnvironment = executionEnvironment;
    return this;
  }

  public ContextFactory warningsLimit(int warningsLimit) {
    this.warningsLimit = warningsLimit;
    return this;
  }

  public ContextFactory options(Map<String, String> options) {
    this.options = options;
    return this;
  }

  PolyglotContext build() {
    if (executionEnvironment != null) {
      options.put("enso.ExecutionEnvironment", executionEnvironment);
    }
    var julLogLevel = Converter.toJavaLevel(logLevel);
    var logLevelName = julLogLevel.getName();
    var builder =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowAllAccess(true)
            .allowHostAccess(new HostAccessFactory().allWithTypeMapping())
            .option(RuntimeOptions.PROJECT_ROOT, projectRoot)
            .option(RuntimeOptions.STRICT_ERRORS, Boolean.toString(strictErrors))
            .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true")
            .option(
                RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION,
                Boolean.toString(useGlobalIrCacheLocation))
            .option(RuntimeOptions.DISABLE_IR_CACHES, Boolean.toString(!enableIrCaches))
            .option(RuntimeOptions.DISABLE_PRIVATE_CHECK, Boolean.toString(disablePrivateCheck))
            .option(DebugServerInfo.ENABLE_OPTION, "true")
            .option(RuntimeOptions.LOG_MASKING, Boolean.toString(logMasking))
            .options(options)
            .option(RuntimeOptions.ENABLE_AUTO_PARALLELISM, Boolean.toString(enableAutoParallelism))
            .option(RuntimeOptions.WARNINGS_LIMIT, Integer.toString(warningsLimit))
            .option("js.foreign-object-prototype", "true")
            .out(out)
            .in(in)
            .serverTransport(
                (uri, peer) ->
                    DebugServerInfo.URI.equals(uri.toString())
                        ? new DebuggerSessionManagerEndpoint(repl, peer)
                        : null);

    builder.option(RuntimeOptions.LOG_LEVEL, logLevelName);
    var logHandler = JulHandler.get();
    var logLevels = LoggerSetup.get().getConfig().getLoggers();
    if (logLevels.hasEnsoLoggers()) {
      logLevels
          .entrySet()
          .forEach(
              (entry) ->
                  builder.option(
                      "log." + LanguageInfo.ID + "." + entry.getKey() + ".level",
                      Converter.toJavaLevel(entry.getValue()).getName()));
    }
    builder.logHandler(logHandler);

    var graalpy =
        new File(
            new File(new File(new File(new File(projectRoot), "polyglot"), "python"), "bin"),
            "graalpy");
    if (graalpy.exists()) {
      builder.option("python.Executable", graalpy.getAbsolutePath());
    }
    if (ENGINE_HAS_JAVA) {
      var javaHome = System.getProperty("java.home");
      if (javaHome == null) {
        javaHome = System.getenv("JAVA_HOME");
      }
      if (javaHome == null) {
        throw new IllegalStateException("Specify JAVA_HOME environment property");
      }
      builder
          .option("java.ExposeNativeJavaVM", "true")
          .option("java.Polyglot", "true")
          .option("java.UseBindingsLoader", "true")
          .option("java.JavaHome", javaHome)
          .allowCreateThread(true);
    }
    return new PolyglotContext(builder.build());
  }

  /**
   * Checks whether the polyglot engine has Espresso. Recorded as static constant to be remembered
   * in AOT mode.
   */
  private static final boolean ENGINE_HAS_JAVA;

  static {
    var modules = ModuleLayer.boot().modules().stream();
    ENGINE_HAS_JAVA = modules.anyMatch(m -> "org.graalvm.espresso".equals(m.getName()));
  }
}
