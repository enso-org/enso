package org.enso.benchmarks.processor;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.stream.Collectors;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.FilerException;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic.Kind;
import org.enso.benchmarks.BenchGroup;
import org.enso.benchmarks.BenchSpec;
import org.enso.benchmarks.ModuleBenchSuite;
import org.enso.benchmarks.Utils;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames.TopScope;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.openide.util.lookup.ServiceProvider;

@SupportedAnnotationTypes("org.enso.benchmarks.processor.GenerateBenchSources")
@ServiceProvider(service = Processor.class)
public class BenchProcessor extends AbstractProcessor {

  private final File ensoHomeOverride;
  private final File ensoDir;
  private File projectRootDir;
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
          "import org.openjdk.jmh.annotations.Benchmark;",
          "import org.openjdk.jmh.annotations.BenchmarkMode;",
          "import org.openjdk.jmh.annotations.Mode;",
          "import org.openjdk.jmh.annotations.Fork;",
          "import org.openjdk.jmh.annotations.Measurement;",
          "import org.openjdk.jmh.annotations.OutputTimeUnit;",
          "import org.openjdk.jmh.annotations.Setup;",
          "import org.openjdk.jmh.annotations.State;",
          "import org.openjdk.jmh.annotations.Scope;",
          "import org.openjdk.jmh.annotations.Warmup;",
          "import org.openjdk.jmh.infra.BenchmarkParams;",
          "import org.openjdk.jmh.infra.Blackhole;",
          "import org.graalvm.polyglot.Context;",
          "import org.graalvm.polyglot.Value;",
          "import org.graalvm.polyglot.io.IOAccess;",
          "import org.enso.polyglot.LanguageInfo;",
          "import org.enso.polyglot.MethodNames;",
          "import org.enso.polyglot.RuntimeOptions;",
          "import org.enso.benchmarks.processor.SpecCollector;",
          "import org.enso.benchmarks.ModuleBenchSuite;",
          "import org.enso.benchmarks.BenchSpec;",
          "import org.enso.benchmarks.BenchGroup;",
          "import org.enso.benchmarks.Utils;");

  public BenchProcessor() {
    ensoDir = Utils.findRepoRootDir();

    // Note that ensoHomeOverride does not have to exist, only its parent directory
    ensoHomeOverride = ensoDir.toPath().resolve("distribution").resolve("component").toFile();
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    var elements = roundEnv.getElementsAnnotatedWith(GenerateBenchSources.class);
    for (var element : elements) {
      GenerateBenchSources annotation = element.getAnnotation(GenerateBenchSources.class);
      projectRootDir = new File(annotation.projectRootPath());
      if (!projectRootDir.exists() || !projectRootDir.isDirectory() || !projectRootDir.canRead()) {
        failWithMessage(
            "Project root dir '"
                + projectRootDir.getAbsolutePath()
                + "' specified in the annotation does not exist or is not readable");
      }
      try (var ctx =
          Context.newBuilder(LanguageInfo.ID)
              .allowExperimentalOptions(true)
              .allowIO(IOAccess.ALL)
              .allowAllAccess(true)
              .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
              .logHandler(System.err)
              .option(RuntimeOptions.PROJECT_ROOT, projectRootDir.getAbsolutePath())
              .option(RuntimeOptions.LANGUAGE_HOME_OVERRIDE, ensoHomeOverride.getAbsolutePath())
              .build()) {
        Value module = getModule(ctx, annotation.moduleName());
        assert module != null;
        List<ModuleBenchSuite> benchSuites =
            SpecCollector.collectBenchSpecsFromModule(module, annotation.variableName());
        for (ModuleBenchSuite benchSuite : benchSuites) {
          for (BenchGroup group : benchSuite.getGroups()) {
            if (!validateGroup(group)) {
              return false;
            } else {
              generateClassForGroup(
                  group, benchSuite.getModuleQualifiedName(), annotation.variableName());
            }
          }
        }
        return true;
      } catch (Exception e) {
        failWithMessage("Uncaught exception in " + getClass().getName() + ": " + e.getMessage());
        return false;
      }
    }
    return true;
  }

  private Value getModule(Context ctx, String moduleName) {
    try {
      return ctx.getBindings(LanguageInfo.ID).invokeMember(TopScope.GET_MODULE, moduleName);
    } catch (PolyglotException e) {
      failWithMessage("Cannot get module '" + moduleName + "': " + e.getMessage());
      return null;
    }
  }

  private void generateClassForGroup(BenchGroup group, String moduleQualifiedName, String varName) {
    String fullClassName = createGroupClassName(group);
    try (PrintWriter srcFileWriter =
        new PrintWriter(processingEnv.getFiler().createSourceFile(fullClassName).openWriter())) {
      generateClassForGroup(srcFileWriter, moduleQualifiedName, varName, group);
    } catch (IOException e) {
      if (!isResourceAlreadyExistsException(e)) {
        failWithMessage(
            "Failed to generate source file for group '" + group.name() + "': " + e.getMessage());
      }
    }
  }

  private boolean validateGroup(BenchGroup group) {
    List<String> specNames =
        group.specs().stream().map(BenchSpec::name).collect(Collectors.toList());
    long distinctNamesCount = specNames.stream().distinct().count();
    List<String> sortedSpecNames = specNames.stream().sorted().collect(Collectors.toList());
    if (specNames.size() != distinctNamesCount) {
      failWithMessage(
          "All benchmark suite names in group '"
              + group.name()
              + "' must be unique."
              + " Found names of the bench suites: "
              + sortedSpecNames);
      return false;
    } else {
      return true;
    }
  }

  /**
   * Returns true iff the given exception is thrown because a file already exists exception. There
   * is no better way to check this.
   *
   * @param e Exception to check.
   * @return true iff the given exception is thrown because a file already exists exception.
   */
  private static boolean isResourceAlreadyExistsException(IOException e) {
    List<String> messages =
        List.of(
            "Source file already created",
            "Resource already created",
            "Attempt to recreate a file");
    return e instanceof FilerException
        && messages.stream().anyMatch(msg -> e.getMessage().contains(msg));
  }

  private void generateClassForGroup(
      PrintWriter out, String moduleQualifiedName, String varName, BenchGroup group)
      throws IOException {
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
    out.println("      .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())");
    out.println("      .logHandler(System.err)");
    out.println("      .option(");
    out.println("        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,");
    out.println("        languageHomeOverride.getAbsolutePath()");
    out.println("      )");
    out.println("      .option(");
    out.println("        RuntimeOptions.PROJECT_ROOT,");
    out.println("        projectRootDir.getAbsolutePath()");
    out.println("      )");
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

  private void failWithMessage(String msg) {
    processingEnv.getMessager().printMessage(Kind.ERROR, msg);
  }
}
