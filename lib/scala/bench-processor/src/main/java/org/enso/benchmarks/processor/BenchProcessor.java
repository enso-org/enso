package org.enso.benchmarks.processor;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Set;
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
          "import org.openjdk.jmh.annotations.Benchmark;",
          "import org.openjdk.jmh.annotations.BenchmarkMode;",
          "import org.openjdk.jmh.annotations.Mode;",
          "import org.openjdk.jmh.annotations.Fork;",
          "import org.openjdk.jmh.annotations.Measurement;",
          "import org.openjdk.jmh.annotations.OutputTimeUnit;",
          "import org.openjdk.jmh.annotations.Setup;",
          "import org.openjdk.jmh.annotations.State;",
          "import org.openjdk.jmh.annotations.Scope;",
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
    File currentDir = null;
    try {
      currentDir =
          new File(
              BenchProcessor.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    } catch (URISyntaxException e) {
      failWithMessage("ensoDir not found: " + e.getMessage());
    }
    for (; currentDir != null; currentDir = currentDir.getParentFile()) {
      if (currentDir.getName().equals("enso")) {
        break;
      }
    }
    if (currentDir == null) {
      failWithMessage("Unreachable: Could not find Enso root directory");
    }
    ensoDir = currentDir;

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
          Context.newBuilder()
              .allowExperimentalOptions(true)
              .allowIO(IOAccess.ALL)
              .allowAllAccess(true)
              .logHandler(new ByteArrayOutputStream())
              .option(RuntimeOptions.PROJECT_ROOT, projectRootDir.getAbsolutePath())
              .option(RuntimeOptions.LANGUAGE_HOME_OVERRIDE, ensoHomeOverride.getAbsolutePath())
              .build()) {
        Value module = getModule(ctx, annotation.moduleName());
        assert module != null;
        List<ModuleBenchSuite> benchSuites =
            SpecCollector.collectBenchSpecsFromModule(module, annotation.variableName());
        for (ModuleBenchSuite benchSuite : benchSuites) {
          for (BenchGroup group : benchSuite.getGroups()) {
            generateClassForGroup(
                group, benchSuite.getModuleQualifiedName(), annotation.variableName());
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
    try (Writer srcFileWriter =
        processingEnv.getFiler().createSourceFile(fullClassName).openWriter()) {
      generateClassForGroup(srcFileWriter, moduleQualifiedName, varName, group);
    } catch (IOException e) {
      if (!isResourceAlreadyExistsException(e)) {
        failWithMessage(
            "Failed to generate source file for group '" + group.name() + "': " + e.getMessage());
      }
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
      Writer javaSrcFileWriter, String moduleQualifiedName, String varName, BenchGroup group)
      throws IOException {
    String groupFullClassName = createGroupClassName(group);
    String className = groupFullClassName.substring(groupFullClassName.lastIndexOf('.') + 1);
    List<BenchSpec> specs = group.specs();
    List<String> specJavaNames =
        specs.stream().map(spec -> normalize(spec.name())).collect(Collectors.toUnmodifiableList());

    javaSrcFileWriter.append("package " + generatedSourcesPackagePrefix + ";\n");
    javaSrcFileWriter.append("\n");
    javaSrcFileWriter.append(String.join("\n", imports));
    javaSrcFileWriter.append("\n");
    javaSrcFileWriter.append("\n");
    javaSrcFileWriter.append("/**\n");
    javaSrcFileWriter.append(" * Generated from:\n");
    javaSrcFileWriter.append(" * - Module: " + moduleQualifiedName + "\n");
    javaSrcFileWriter.append(" * - Group: \"" + group.name() + "\"\n");
    javaSrcFileWriter.append(" * Generated by {@link " + getClass().getName() + "}.\n");
    javaSrcFileWriter.append(" */\n");
    javaSrcFileWriter.append("@BenchmarkMode(Mode.AverageTime)\n");
    javaSrcFileWriter.append("@Fork(1)\n");
    javaSrcFileWriter.append("@State(Scope.Benchmark)\n");
    javaSrcFileWriter.append("public class " + className + " {\n");
    javaSrcFileWriter.append("  private Value groupInputArg;\n");
    for (var specJavaName : specJavaNames) {
      javaSrcFileWriter.append("  private Value benchFunc_" + specJavaName + ";\n");
    }
    javaSrcFileWriter.append("  \n");
    javaSrcFileWriter.append("  @Setup\n");
    javaSrcFileWriter.append("  public void setup(BenchmarkParams params) throws Exception {\n");
    javaSrcFileWriter
        .append("    File projectRootDir = Utils.findRepoRootDir().toPath().resolve(\"")
        .append(projectRootDir.toString())
        .append("\").toFile();\n");
    javaSrcFileWriter.append(
        "    if (projectRootDir == null || !projectRootDir.exists() || !projectRootDir.canRead()) {\n");
    javaSrcFileWriter.append(
        "      throw new IllegalStateException(\"Project root directory does not exist or cannot be read: \" + Objects.toString(projectRootDir));\n");
    javaSrcFileWriter.append("    }\n");
    javaSrcFileWriter.append("    File languageHomeOverride = Utils.findLanguageHomeOverride();\n");
    javaSrcFileWriter.append("    var ctx = Context.newBuilder()\n");
    javaSrcFileWriter.append("      .allowExperimentalOptions(true)\n");
    javaSrcFileWriter.append("      .allowIO(IOAccess.ALL)\n");
    javaSrcFileWriter.append("      .allowAllAccess(true)\n");
    javaSrcFileWriter.append("      .logHandler(new ByteArrayOutputStream())\n");
    javaSrcFileWriter.append("      .option(\n");
    javaSrcFileWriter.append("        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,\n");
    javaSrcFileWriter.append("        languageHomeOverride.getAbsolutePath()\n");
    javaSrcFileWriter.append("      )\n");
    javaSrcFileWriter.append("      .option(\n");
    javaSrcFileWriter.append("        RuntimeOptions.PROJECT_ROOT,\n");
    javaSrcFileWriter.append("        projectRootDir.getAbsolutePath()\n");
    javaSrcFileWriter.append("      )\n");
    javaSrcFileWriter.append("      .build();\n");
    javaSrcFileWriter.append("    \n");
    javaSrcFileWriter.append("    Value bindings = ctx.getBindings(LanguageInfo.ID);\n");
    javaSrcFileWriter.append(
        "    Value module = bindings.invokeMember(MethodNames.TopScope.GET_MODULE, \""
            + moduleQualifiedName
            + "\");\n");
    javaSrcFileWriter.append(
        "    BenchGroup group = SpecCollector.collectBenchGroupFromModule(module, \""
            + group.name()
            + "\", \""
            + varName
            + "\");\n");
    javaSrcFileWriter.append("    \n");
    for (int i = 0; i < specs.size(); i++) {
      var specJavaName = specJavaNames.get(i);
      var specName = specs.get(i).name();
      javaSrcFileWriter.append(
          "    BenchSpec benchSpec_"
              + specJavaName
              + " = Utils.findSpecByName(group, \""
              + specName
              + "\");\n");
      javaSrcFileWriter.append(
          "    this.benchFunc_" + specJavaName + " = benchSpec_" + specJavaName + ".code();\n");
    }
    javaSrcFileWriter.append("    \n");
    javaSrcFileWriter.append("    this.groupInputArg = Value.asValue(null);\n");
    javaSrcFileWriter.append("  } \n"); // end of setup method
    javaSrcFileWriter.append("  \n");

    // Benchmark methods
    for (var specJavaName : specJavaNames) {
      javaSrcFileWriter.append("  \n");
      javaSrcFileWriter.append("  @Benchmark\n");
      javaSrcFileWriter.append("  public void " + specJavaName + "(Blackhole blackhole) {\n");
      javaSrcFileWriter.append(
          "    Value result = this.benchFunc_" + specJavaName + ".execute(this.groupInputArg);\n");
      javaSrcFileWriter.append("    blackhole.consume(result);\n");
      javaSrcFileWriter.append("  }\n"); // end of benchmark method
    }

    javaSrcFileWriter.append("}\n"); // end of class className
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
