package org.enso.benchmarks.processor;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.enso.benchmarks.BenchGroup;
import org.enso.benchmarks.BenchSpec;
import org.enso.benchmarks.BenchSuite;
import org.enso.benchmarks.ModuleBenchSuite;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.common.MethodNames.TopScope;
import org.enso.common.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;

/**
 * Collect benchmark specifications from Enso source files. Ensure that this class is loaded by the
 * same class loader used for loading the {@code org.graalvm.truffle} module. Otherwise, you might
 * encounter issues with double class loading when Enso code is executed when collecting specs.
 */
public final class SpecCollector {
  private final File projectRootDir;
  private final File ensoHomeOverride;
  private final String moduleName;
  private final String varName;
  private final BiConsumer<String, String> createSourceFileFn;
  private static final String generatedSourcesPackagePrefix = "org.enso.benchmarks.generated";

  private static final List<String> imports =
      List.of(
          "import java.nio.file.Paths;",
          "import java.io.ByteArrayOutputStream;",
          "import java.io.File;",
          "import java.util.List;",
          "import java.util.Objects;",
          "import java.util.concurrent.TimeUnit;",
          "import java.util.logging.Level;",
          "import java.util.logging.LogRecord;",
          "import java.util.logging.Handler;",
          "import org.openjdk.jmh.annotations.Benchmark;",
          "import org.openjdk.jmh.annotations.BenchmarkMode;",
          "import org.openjdk.jmh.annotations.Mode;",
          "import org.openjdk.jmh.annotations.Fork;",
          "import org.openjdk.jmh.annotations.Measurement;",
          "import org.openjdk.jmh.annotations.OutputTimeUnit;",
          "import org.openjdk.jmh.annotations.Setup;",
          "import org.openjdk.jmh.annotations.TearDown;",
          "import org.openjdk.jmh.annotations.State;",
          "import org.openjdk.jmh.annotations.Scope;",
          "import org.openjdk.jmh.annotations.Warmup;",
          "import org.openjdk.jmh.infra.BenchmarkParams;",
          "import org.openjdk.jmh.infra.IterationParams;",
          "import org.openjdk.jmh.infra.Blackhole;",
          "import org.graalvm.polyglot.Context;",
          "import org.graalvm.polyglot.Value;",
          "import org.graalvm.polyglot.io.IOAccess;",
          "import org.enso.common.LanguageInfo;",
          "import org.enso.common.MethodNames;",
          "import org.enso.common.RuntimeOptions;",
          "import org.enso.benchmarks.processor.SpecCollector;",
          "import org.enso.benchmarks.ModuleBenchSuite;",
          "import org.enso.benchmarks.BenchSpec;",
          "import org.enso.benchmarks.BenchGroup;",
          "import org.enso.benchmarks.Utils;");

  private SpecCollector(
      String moduleName,
      String varName,
      File projectRootDir,
      File ensoHomeOverride,
      BiConsumer<String, String> createSourceFileFn) {
    this.projectRootDir = projectRootDir;
    this.ensoHomeOverride = ensoHomeOverride;
    this.moduleName = moduleName;
    this.varName = varName;
    this.createSourceFileFn = createSourceFileFn;
  }

  /**
   * Invoked via reflection from {@code BenchProcessor}. Ensure that the class loader used for
   * loading this class, and this method, is the same as the one used for loading the {@code
   * org.graalvm.truffle} module.
   *
   * @param moduleName Fully qualified name of the module that contains the bench specs
   * @param varName Name of the variable that holds all the collected bench suites
   * @param projectRootDir Root directory of the project. Must be a directory and exist.
   * @param ensoHomeOverride Path to the Enso home directory. Will be used for polyglot context.
   */
  public static SpecCollector create(
      String moduleName,
      String varName,
      File projectRootDir,
      File ensoHomeOverride,
      BiConsumer<String, String> createSourceFileFn) {
    if (!projectRootDir.exists() || !projectRootDir.isDirectory() || !projectRootDir.canRead()) {
      throw new IllegalArgumentException(
          "Project root directory " + projectRootDir + "does not exist or is not readable.");
    }
    return new SpecCollector(
        moduleName, varName, projectRootDir, ensoHomeOverride, createSourceFileFn);
  }

  /**
   * Invoked via reflection from {@code BenchProcessor}. Ensure that the class loader used for
   * loading this class, and this method, is the same as the one used for loading the {@code
   * org.graalvm.truffle} module.
   *
   * @throws SpecCollectionException
   */
  public void generateBenchSpecs() throws SpecCollectionException {
    if (!projectRootDir.exists() || !projectRootDir.isDirectory() || !projectRootDir.canRead()) {
      throw new IllegalArgumentException(
          "Project root directory " + projectRootDir + "does not exist or is not readable.");
    }
    try (var ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(new ConsoleHandler())
            .option(RuntimeOptions.PROJECT_ROOT, projectRootDir.getAbsolutePath())
            .option(RuntimeOptions.LANGUAGE_HOME_OVERRIDE, ensoHomeOverride.getAbsolutePath())
            .build()) {
      var langs = ctx.getEngine().getLanguages().keySet();
      Value module = getModule(ctx, moduleName);
      assert module != null;
      List<ModuleBenchSuite> benchSuites =
          SpecCollector.collectBenchSpecsFromModule(module, varName);
      for (ModuleBenchSuite benchSuite : benchSuites) {
        for (BenchGroup group : benchSuite.getGroups()) {
          validateGroup(group);
          generateClassForGroup(group, benchSuite.getModuleQualifiedName(), varName);
        }
      }
    } catch (Exception e) {
      throw new SpecCollectionException(
          "Uncaught exception in " + getClass().getName() + ": " + e.getMessage(), e);
    }
  }

  /**
   * Collects all the bench specifications from the given module in a variable with the given name.
   *
   * @param varName Name of the variable that holds all the collected bench suites.
   * @return Empty list if no such variable exists, or if it is not a vector.
   */
  public static List<ModuleBenchSuite> collectBenchSpecsFromModule(Value module, String varName) {
    Value moduleType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    Value allSuitesVar = module.invokeMember(Module.GET_METHOD, moduleType, varName);
    String moduleQualifiedName = module.invokeMember(Module.GET_NAME).asString();
    if (!allSuitesVar.isNull()) {
      Value suitesValue = module.invokeMember(Module.EVAL_EXPRESSION, varName);
      if (!suitesValue.hasArrayElements()) {
        return List.of();
      }
      List<ModuleBenchSuite> suites = new ArrayList<>();
      for (long i = 0; i < suitesValue.getArraySize(); i++) {
        Value suite = suitesValue.getArrayElement(i);
        BenchSuite benchSuite = suite.as(BenchSuite.class);
        suites.add(new ModuleBenchSuite(benchSuite, moduleQualifiedName));
      }
      return suites;
    }
    return List.of();
  }

  /**
   * Collects all the bench specifications from the given module in a variable with the given name.
   *
   * @param groupName Name of the benchmark group
   * @param varName Name of the variable that holds all the collected bench suites.
   * @return null if no such group exists.
   */
  public static BenchGroup collectBenchGroupFromModule(
      Value module, String groupName, String varName) {
    var specs = collectBenchSpecsFromModule(module, varName);
    for (ModuleBenchSuite suite : specs) {
      BenchGroup group = suite.findGroupByName(groupName);
      if (group != null) {
        return group;
      }
    }
    return null;
  }

  private Value getModule(Context ctx, String moduleName) {
    try {
      return ctx.getBindings(LanguageInfo.ID).invokeMember(TopScope.GET_MODULE, moduleName);
    } catch (PolyglotException e) {
      throw new SpecCollectionException(
          "Cannot get module '" + moduleName + "': " + e.getMessage(), e);
    }
  }

  private void generateClassForGroup(BenchGroup group, String moduleQualifiedName, String varName) {
    String fullClassName = createGroupClassName(group);
    var sw = new StringWriter();
    try (PrintWriter srcFileWriter = new PrintWriter(sw)) {
      generateClassForGroup(srcFileWriter, moduleQualifiedName, varName, group);
    }
    createSourceFileFn.accept(fullClassName, sw.toString());
  }

  private void validateGroup(BenchGroup group) {
    List<String> specNames =
        group.specs().stream().map(BenchSpec::name).collect(Collectors.toList());
    long distinctNamesCount = specNames.stream().distinct().count();
    List<String> sortedSpecNames = specNames.stream().sorted().collect(Collectors.toList());
    if (specNames.size() != distinctNamesCount) {
      throw new SpecCollectionException(
          "All benchmark suite names in group '"
              + group.name()
              + "' must be unique."
              + " Found names of the bench suites: "
              + sortedSpecNames);
    }
  }

  private void generateClassForGroup(
      PrintWriter out, String moduleQualifiedName, String varName, BenchGroup group) {
    String groupFullClassName = createGroupClassName(group);
    String className = groupFullClassName.substring(groupFullClassName.lastIndexOf('.') + 1);
    List<BenchSpec> specs = group.specs();
    List<String> specJavaNames =
        specs.stream().map(spec -> normalize(spec.name())).collect(Collectors.toUnmodifiableList());
    out.println("package " + generatedSourcesPackagePrefix + ";");
    out.println();
    out.println(String.join("\n", imports));
    out.println();
    out.println("/**");
    out.println(" * Generated from:");
    out.println(" * - Module: " + moduleQualifiedName);
    out.println(" * - Group: \"" + group.name() + "\"");
    out.println(" * Generated by {@link " + getClass().getName() + "}.");
    out.println(" */");
    out.println("@BenchmarkMode(Mode.AverageTime)");
    out.println("@OutputTimeUnit(TimeUnit.MILLISECONDS)");
    out.println("@Fork(1)");
    out.println(getWarmupAnnotationForGroup(group));
    out.println(getMeasureAnnotationForGroup(group));
    out.println("@State(Scope.Benchmark)");
    out.println("public class " + className + " {");

    // Field definitions
    out.println("  private int warmupCounter = 0;");
    out.println("  private int measurementCounter = 0;");
    out.println("  private boolean compilationMessagesFound;");
    out.println("  private final StringBuilder compilationLog = new StringBuilder();");
    out.println(
        "  private final List<LogRecord> messages = new"
            + " java.util.concurrent.CopyOnWriteArrayList<>();");
    out.println("  private Value groupInputArg;");
    for (var specJavaName : specJavaNames) {
      out.println("  private Value benchFunc_" + specJavaName + ";");
    }
    out.println("  ");
    out.println("  @Setup");
    out.println("  public void setup(BenchmarkParams params) throws Exception {");
    // Workaround for compilation failures on Windows.
    String projectRootDirPath =
        projectRootDir.getPath().contains("\\")
            ? projectRootDir.getPath().replace("\\", "\\\\")
            : projectRootDir.getPath();
    out.append("    File projectRootDir = Utils.findRepoRootDir().toPath().resolve(\"")
        .append(projectRootDirPath)
        .append("\").toFile();\n");
    out.println(
        "    if (projectRootDir == null || !projectRootDir.exists() || !projectRootDir.canRead())"
            + " {");
    out.println(
        "      throw new IllegalStateException(\"Project root directory does not exist or cannot be"
            + " read: \" + Objects.toString(projectRootDir));");
    out.println("    }");
    out.println("    File languageHomeOverride = Utils.findLanguageHomeOverride();");
    out.println("    var ctx = Context.newBuilder()");
    out.println("      .allowExperimentalOptions(true)");
    out.println("      .allowIO(IOAccess.ALL)");
    out.println("      .allowAllAccess(true)");
    out.println("      .option(RuntimeOptions.LOG_LEVEL, Level.FINE.getName())");
    out.println("      .logHandler(System.err)");
    out.println("      .option(");
    out.println("        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,");
    out.println("        languageHomeOverride.getAbsolutePath()");
    out.println("      )");
    out.println("      .option(");
    out.println("        RuntimeOptions.PROJECT_ROOT,");
    out.println("        projectRootDir.getAbsolutePath()");
    out.println("      )");
    out.println(
        """
                      .option("engine.TraceCompilation", "true")
                      .logHandler(new java.util.logging.Handler() {
                         @Override
                         public void publish(LogRecord lr) {
                           if ("engine".equals(lr.getLoggerName())) {
                             messages.add(lr);
                           }
                         }
                         @Override public void flush() {}
                         @Override public void close() {}
                      })
                """);
    out.println("      .build();");
    out.println("    ");
    out.println("    Value bindings = ctx.getBindings(LanguageInfo.ID);");
    out.println(
        "    Value module = bindings.invokeMember(MethodNames.TopScope.GET_MODULE, \""
            + moduleQualifiedName
            + "\");");
    out.println(
        "    BenchGroup group = SpecCollector.collectBenchGroupFromModule(module, \""
            + group.name()
            + "\", \""
            + varName
            + "\");");
    out.println("    ");
    for (int i = 0; i < specs.size(); i++) {
      var specJavaName = specJavaNames.get(i);
      var specName = specs.get(i).name();
      out.println(
          "    BenchSpec benchSpec_"
              + specJavaName
              + " = Utils.findSpecByName(group, \""
              + specName
              + "\");");
      out.println(
          "    this.benchFunc_" + specJavaName + " = benchSpec_" + specJavaName + ".code();");
    }
    out.println("    ");
    out.println("    this.groupInputArg = Value.asValue(null);");
    out.println("  } "); // end of setup method
    out.println("  ");

    out.println(
        """
                  @Setup(org.openjdk.jmh.annotations.Level.Iteration)
                  public void clearCompilationMessages(IterationParams it) {
                    var round = round(it);
                    if (!messages.isEmpty()) {
                      compilationLog.append("Before " + it.getType() + "#" + round + ". ");
                      compilationLog.append("Cleaning " + messages.size() + " compilation messages\\n");
                      messages.clear();
                    }
                  }

                  private int round(IterationParams it) {
                    return switch (it.getType()) {
                      case WARMUP -> ++warmupCounter;
                      case MEASUREMENT -> ++measurementCounter;
                    };
                  }

                  private void dumpMessages() {
                    for (var lr : messages) {
                      compilationLog.append(lr.getMessage() + "\\n");
                      compilationMessagesFound = true;
                    }
                  }

                  @TearDown(org.openjdk.jmh.annotations.Level.Iteration)
                  public void dumpCompilationMessages(IterationParams it) {
                    switch (it.getType()) {
                      case MEASUREMENT -> {
                        compilationLog.append("After " + it.getType() + "#" + measurementCounter + ". ");
                        if (!messages.isEmpty()) {
                          compilationLog.append("Dumping " + messages.size() + " compilation messages:\\n");
                          dumpMessages();
                        } else {
                          compilationLog.append("No compilation messages.\\n");
                        }
                      }
                    }
                  }

                  @TearDown
                  public void checkNoTruffleCompilation(BenchmarkParams params) {
                    if (compilationMessagesFound) {
                      System.err.println(compilationLog.toString());
                    }
                  }

                """);
    // Benchmark methods
    for (var specJavaName : specJavaNames) {
      out.println();
      out.println("  @Benchmark");
      out.println("  public void " + specJavaName + "(Blackhole blackhole) {");
      out.println(
          "    Value result = this.benchFunc_" + specJavaName + ".execute(this.groupInputArg);");
      out.println("    blackhole.consume(result);");
      out.println("  }"); // end of benchmark method
    }

    out.println("}"); // end of class className
  }

  private String getWarmupAnnotationForGroup(BenchGroup group) {
    var warmupConf = group.configuration().warmup();
    return """
    @Warmup(
      iterations = $1,
      time = $2,
      timeUnit = TimeUnit.SECONDS
    )
    """
        .strip()
        .replace("$1", Long.toString(warmupConf.iterations()))
        .replace("$2", Long.toString(warmupConf.seconds()));
  }

  private String getMeasureAnnotationForGroup(BenchGroup group) {
    var measureConf = group.configuration().measure();
    return """
    @Measurement(
      iterations = $1,
      time = $2,
      timeUnit = TimeUnit.SECONDS
    )
    """
        .strip()
        .replace("$1", Long.toString(measureConf.iterations()))
        .replace("$2", Long.toString(measureConf.seconds()));
  }

  /**
   * Returns Java FQN for a benchmark spec.
   *
   * @param group Group name will be converted to Java package name.
   * @return
   */
  private static String createGroupClassName(BenchGroup group) {
    var groupPkgName = normalize(group.name());
    return generatedSourcesPackagePrefix + "." + groupPkgName;
  }

  private static boolean isValidChar(char c) {
    return Character.isAlphabetic(c) || Character.isDigit(c) || c == '_';
  }

  /**
   * Converts Text to valid Java identifier.
   *
   * @param name Text to convert.
   * @return Valid Java identifier, non null.
   */
  private static String normalize(String name) {
    var normalizedNameSb = new StringBuilder();
    for (char c : name.toCharArray()) {
      if (isValidChar(c)) {
        normalizedNameSb.append(c);
      } else if (c == '-') {
        normalizedNameSb.append("minus");
      } else if (Character.isWhitespace(c) && (peekLastChar(normalizedNameSb) != '_')) {
        normalizedNameSb.append('_');
      }
    }
    return normalizedNameSb.toString();
  }

  private static char peekLastChar(StringBuilder sb) {
    if (!sb.isEmpty()) {
      return sb.charAt(sb.length() - 1);
    } else {
      return 0;
    }
  }
}
