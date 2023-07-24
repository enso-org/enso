package org.enso.interpreter.dsl;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.processing.Filer;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;

import org.enso.interpreter.dsl.model.MethodDefinition;
import org.enso.interpreter.dsl.model.MethodDefinition.ArgumentDefinition;
import org.openide.util.lookup.ServiceProvider;

/**
 * The processor used to generate code from the {@link BuiltinMethod} annotation and collect
 * metadata necessary for automatic builtin methods initialization.
 */
@SupportedAnnotationTypes("org.enso.interpreter.dsl.BuiltinMethod")
@ServiceProvider(service = Processor.class)
public class MethodProcessor extends BuiltinsMetadataProcessor<MethodProcessor.MethodMetadataEntry> {

  private final Map<Filer, Map<String, String[]>> builtinMethods = new HashMap<>();

  /**
   * Processes annotated elements, generating code for each of them. The method also records
   * information about builtin method in an internal map that will be dumped on the last round of
   * processing.
   *
   * @param annotations annotation being processed this round.
   * @param roundEnv additional round information.
   * @return {@code true}
   */
  @Override
  public boolean handleProcess(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      for (Element elt : annotatedElements) {
        if (elt.getKind() == ElementKind.CLASS) {
          try {
            var needsFrame = BuiltinsProcessor.checkNeedsFrame(elt);
            handleTypeElement((TypeElement) elt, roundEnv, needsFrame);
          } catch (IOException e) {
            processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, e.getMessage());
          }

        } else {
          processingEnv
              .getMessager()
              .printMessage(
                  Diagnostic.Kind.ERROR,
                  "Invalid use of " + annotation.getSimpleName() + " with " + elt.getKind());
        }
      }
    }
    return true;
  }

  private void handleTypeElement(TypeElement element, RoundEnvironment roundEnv, Boolean needsFrame)
      throws IOException {
    ExecutableElement executeMethod =
        element.getEnclosedElements().stream()
            .filter(
                x -> {
                  if (!(x instanceof ExecutableElement)) return false;
                  Name name = x.getSimpleName();
                  return name.contentEquals("execute");
                })
            .map(x -> (ExecutableElement) x)
            .findFirst()
            .orElseGet(
                () -> {
                  processingEnv
                      .getMessager()
                      .printMessage(Diagnostic.Kind.ERROR, "No execute method found.", element);
                  return null;
                });
    if (executeMethod == null) return;
    String pkgName =
        processingEnv.getElementUtils().getPackageOf(element).getQualifiedName().toString();

    MethodDefinition def = new MethodDefinition(pkgName, element, executeMethod, needsFrame);
    if (!def.validate(processingEnv)) {
      return;
    }
    generateCode(def);
    String tpe = def.getType().toLowerCase();
    if (tpe.isEmpty()) {
      processingEnv
          .getMessager()
          .printMessage(
              Diagnostic.Kind.ERROR,
              "Type of the BuiltinMethod cannot be empty in: " + def.getClassName());
      return;
    }
    String fullClassName = def.getPackageName() + "." + def.getClassName();
    registerBuiltinMethod(processingEnv.getFiler(), def.getDeclaredName(), fullClassName, def.isStatic(), def.isAutoRegister());
    if (def.hasAliases()) {
      for (String alias : def.aliases()) {
        registerBuiltinMethod(processingEnv.getFiler(), alias, fullClassName, def.isStatic(), def.isAutoRegister());
      }
    }
  }

  private final List<String> necessaryImports =
      Arrays.asList(
          "com.oracle.truffle.api.frame.VirtualFrame",
          "com.oracle.truffle.api.nodes.NodeInfo",
          "com.oracle.truffle.api.nodes.RootNode",
          "com.oracle.truffle.api.nodes.UnexpectedResultException",
          "com.oracle.truffle.api.profiles.BranchProfile",
          "com.oracle.truffle.api.profiles.CountingConditionProfile",
          "java.nio.file.OpenOption",
          "org.enso.interpreter.EnsoLanguage",
          "org.enso.interpreter.node.InlineableNode",
          "org.enso.interpreter.node.expression.builtin.BuiltinRootNode",
          "org.enso.interpreter.runtime.callable.argument.ArgumentDefinition",
          "org.enso.interpreter.runtime.callable.function.Function",
          "org.enso.interpreter.runtime.callable.function.FunctionSchema",
          "org.enso.interpreter.runtime.EnsoContext",
          "org.enso.interpreter.runtime.data.ArrayRope",
          "org.enso.interpreter.runtime.error.PanicException",
          "org.enso.interpreter.runtime.error.Warning",
          "org.enso.interpreter.runtime.error.WithWarnings",
          "org.enso.interpreter.runtime.state.State",
          "org.enso.interpreter.runtime.type.TypesGen");

  private void generateCode(MethodDefinition methodDefinition) throws IOException {
    JavaFileObject gen =
        processingEnv.getFiler().createSourceFile(methodDefinition.getQualifiedName());
    Set<String> allImports = new HashSet<>(necessaryImports);
    allImports.addAll(methodDefinition.getImports());

    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + methodDefinition.getPackageName() + ";");
      out.println();

      allImports.forEach(pkg -> out.println("import " + pkg + ";"));

      out.println();

      out.println("/**");
      out.println(" * Generated by {@link " + getClass().getName() + "}.");
      out.println(" * From {@link " + methodDefinition.getOriginalClassName() + "}.");
      out.println(" */");
      out.println("@NodeInfo(");
      out.println("  shortName = \"" + methodDefinition.getDeclaredName() + "\",");
      out.println("  description = \"\"\"\n" + methodDefinition.getDescription() + "\"\"\")");
      if (methodDefinition.needsFrame()) {
        out.println("public class " + methodDefinition.getClassName() + " extends BuiltinRootNode {");
      } else {
        out.println("public class " + methodDefinition.getClassName() + " extends BuiltinRootNode implements InlineableNode.Root {");
      }
      out.println("  private @Child " + methodDefinition.getOriginalClassName() + " bodyNode;");
      out.println();
      out.println("  private static final class Internals {");
      out.println("    Internals(boolean s) {");
      out.println("      this.staticOfInstanceMethod = s;");
      out.println("    }");
      out.println();
      out.println("    private final boolean staticOfInstanceMethod;");

      for (MethodDefinition.ArgumentDefinition arg : methodDefinition.getArguments()) {
        if (arg.shouldCheckErrors()) {
          String condName = mkArgumentInternalVarName(arg) + DATAFLOW_ERROR_PROFILE;
          out.println(
              "    private final CountingConditionProfile "
                  + condName
                  + " = CountingConditionProfile.create();");
        }

        if (arg.isPositional() && !arg.isSelf()) {
          String branchName = mkArgumentInternalVarName(arg) + PANIC_SENTINEL_PROFILE;
          out.println("    private final BranchProfile " + branchName + " = BranchProfile.create();");
        }

        if (arg.shouldCheckWarnings()) {
          String warningName = mkArgumentInternalVarName(arg) + WARNING_PROFILE;
          out.println(
              "    private final BranchProfile " + warningName + " = BranchProfile.create();");
        }
      }
      out.println("    private final BranchProfile anyWarningsProfile = BranchProfile.create();");
      out.println("  }");
      out.println("  private final Internals internals;");

      out.println("  private " + methodDefinition.getClassName() + "(EnsoLanguage language, boolean staticOfInstanceMethod) {");
      out.println("    super(language);");
      out.println("    this.bodyNode = " + methodDefinition.getConstructorExpression() + ";");
      out.println("    this.internals = new Internals(staticOfInstanceMethod);");
      out.println("  }");

      out.println();

      String functionBuilderMethod =
          methodDefinition.needsCallerInfo()
              ? "fromBuiltinRootNodeWithCallerFrameAccess"
              : "fromBuiltinRootNode";

      out.println("  public static Function makeFunction(EnsoLanguage language) {");
      out.println("    return makeFunction(language, false);");
      out.println("  }");
      out.println();
      out.println("  public static Function makeFunction(EnsoLanguage language, boolean staticOfInstanceMethod) {");
      out.println("    if (staticOfInstanceMethod) {");
      out.println("      return Function." + functionBuilderMethod + "(");
      out.print("        new " + methodDefinition.getClassName() + "(language, staticOfInstanceMethod)");
      List<String> argsStaticInstace = generateMakeFunctionArgs(true, methodDefinition.getArguments());
      if (!argsStaticInstace.isEmpty()) {
        out.println(",");
      }
      out.println(String.join(",\n", argsStaticInstace) + ");");
      out.println("    } else {");
      out.println("      return Function." + functionBuilderMethod + "(");
      out.print("        new " + methodDefinition.getClassName() + "(language, staticOfInstanceMethod)");
      List<String> argsInstance = generateMakeFunctionArgs(false, methodDefinition.getArguments());
      if (!argsInstance.isEmpty()) {
        out.println(",");
      }
      out.println(String.join(",\n", argsInstance) + ");");
      out.println("    }");
      out.println("  }");

      out.println();
      if (!methodDefinition.needsFrame()) {
        out.println("  @Override");
        out.println("  public final InlineableNode createInlineableNode() {");
        out.println("    class Inlineable extends InlineableNode {");
        out.println("      private final Internals extra = new Internals(internals.staticOfInstanceMethod);");
        out.println("      private @Child " + methodDefinition.getOriginalClassName() + " body = " + methodDefinition.getConstructorExpression() + ";");
        out.println("      @Override");
        out.println("      public Object call(VirtualFrame frame, Object[] args) {");
        out.println("        return handleExecute(frame, extra, body, args);");
        out.println("      }");
        out.println("    }");
        out.println("    return new Inlineable();");
        out.println("  }");
      }

      out.println("  @Override");
      out.println("  public Object execute(VirtualFrame frame) {");
      if (methodDefinition.needsFrame()) {
        out.println("    var args = frame.getArguments();");
      } else {
        out.println("    return handleExecute(frame, this.internals, bodyNode, frame.getArguments());");
        out.println("  }");
        out.println("  private static Object handleExecute(VirtualFrame frame, Internals internals, " + methodDefinition.getOriginalClassName() + " bodyNode, Object[] args) {");
      }
      out.println("    var prefix = internals.staticOfInstanceMethod ? 1 : 0;");
      out.println("    State state = Function.ArgumentsHelper.getState(args);");
      if (methodDefinition.needsCallerInfo()) {
        out.println(
            "    CallerInfo callerInfo = Function.ArgumentsHelper.getCallerInfo(args);");
      }
      out.println(
          "    Object[] arguments = Function.ArgumentsHelper.getPositionalArguments(args);");
      List<String> callArgNames = new ArrayList<>();
      for (MethodDefinition.ArgumentDefinition arg :
              methodDefinition.getArguments()) {
        if (!(arg.isImplicit() || arg.isFrame() || arg.isState() || arg.isCallerInfo())) {
          out.println("    int arg" + arg.getPosition() + "Idx = " + arg.getPosition() + " + prefix;");
        }
      }
      boolean warningsPossible =
          generateWarningsCheck(out, methodDefinition.getArguments(), "arguments");
      for (MethodDefinition.ArgumentDefinition argumentDefinition :
          methodDefinition.getArguments()) {
        out.println("    /***  Start of processing argument " + argumentDefinition.getPosition() + "  ***/");
        if (argumentDefinition.isImplicit()) {
        } else if (argumentDefinition.isState()) {
          callArgNames.add("state");
        } else if (argumentDefinition.isFrame()) {
          callArgNames.add("frame");
        } else if (argumentDefinition.isCallerInfo()) {
          callArgNames.add("callerInfo");
        } else {
          callArgNames.add(mkArgumentInternalVarName(argumentDefinition));
          generateArgumentRead(out, argumentDefinition, "arguments");
        }
        out.println("    /***  End of processing argument " + argumentDefinition.getPosition() + "  ***/");
      }
      String executeCall = "bodyNode.execute(" + String.join(", ", callArgNames) + ")";
      if (warningsPossible) {
        out.println("    if (anyWarnings) {");
        out.println("      internals.anyWarningsProfile.enter();");
        out.println("      Object result = " + executeCall + ";");
        out.println("      EnsoContext ctx = EnsoContext.get(bodyNode);");
        out.println("      return WithWarnings.appendTo(ctx, result, gatheredWarnings);");
        out.println("    } else {");
        out.println("      return " + executeCall + ";");
        out.println("    }");
      } else {
        out.println("    return " + executeCall + ";");
      }
      out.println("  }");

      out.println();

      out.println("  @Override");
      out.println("  public String getName() {");
      out.println("    return \"" + methodDefinition.getDeclaredName() + "\";");
      out.println("  }");

      out.println();

      out.println("  @Override");
      out.println("  public boolean isCloningAllowed() {");
      out.println("    return true;");
      out.println("  }");

      out.println();

      out.println("  @Override");
      out.println("  protected boolean isCloneUninitializedSupported() {");
      out.println("    return true;");
      out.println("  }");

      out.println();

      out.println("  @Override");
      out.println("  protected RootNode cloneUninitialized() {");
      out.println("    return new " + methodDefinition.getClassName() + "(EnsoLanguage.get(this), internals.staticOfInstanceMethod);");
      out.println("  }");

      out.println();

      out.println("}");
    }
  }

  private List<String> generateMakeFunctionArgs(boolean staticInstance, List<ArgumentDefinition> args) {
    List<String> argumentDefs = new ArrayList<>();
    int staticPrefix = 0;
    if (staticInstance) {
      argumentDefs.add("        new ArgumentDefinition(0, \"selfStatic\", null, null, ArgumentDefinition.ExecutionMode.EXECUTE)");
      staticPrefix = 1;
    }
    for (MethodDefinition.ArgumentDefinition arg : args) {
      if (arg.isPositional()) {
        String executionMode = arg.isSuspended() ? "PASS_THUNK" : "EXECUTE";
        argumentDefs.add(
                "        new ArgumentDefinition("
                        + (staticPrefix + arg.getPosition())
                        + ", \""
                        + arg.getName()
                        + "\", null, null, ArgumentDefinition.ExecutionMode."
                        + executionMode
                        + ")");
      }
    }
    return argumentDefs;
  }

  private void generateArgumentRead(
      PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String argReference = argsArray + "[arg" + arg.getPosition() + "Idx]";
    if (arg.shouldCheckErrors()) {
      String condProfile = mkArgumentInternalVarName(arg) + DATAFLOW_ERROR_PROFILE;
      out.println(
          "    if (internals."
              + condProfile
              + ".profile(TypesGen.isDataflowError("
              + argReference
              + "))) {\n"
              + "      return "
              + argReference
              + ";\n"
              + "    }");
    }
    if (!arg.isSelf()) {
      String branchProfile = mkArgumentInternalVarName(arg) + PANIC_SENTINEL_PROFILE;
      out.println(
          "    if (TypesGen.isPanicSentinel("
              + argReference
              + ")) {\n"
              + "      internals."
              + branchProfile
              + ".enter();\n"
              + "      throw TypesGen.asPanicSentinel("
              + argReference
              + ");\n"
              + "    }");
    }

    if (!arg.requiresCast()) {
      generateUncastedArgumentRead(out, arg, argsArray);
    } else if (arg.isSelf()) {
      generateUncheckedArgumentRead(out, arg, argsArray);
    } else if (arg.isArray()) {
      generateUncheckedArrayCast(out, arg, argsArray);
    } else {
      generateCheckedArgumentRead(out, arg, argsArray);
    }
  }

  private void generateUncastedArgumentRead(
      PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String varName = mkArgumentInternalVarName(arg);
    out.println(
        "    "
            + arg.getTypeName()
            + " "
            + varName
            + " = "
            + argsArray
            + "[arg"
            + arg.getPosition()
            + "Idx];");
  }

  private void generateUncheckedArgumentRead(
      PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String castName = "TypesGen.as" + capitalize(arg.getTypeName());
    String varName = mkArgumentInternalVarName(arg);
    out.println(
        "    "
            + arg.getTypeName()
            + " "
            + varName
            + " = "
            + castName
            + "("
            + argsArray
            + "[arg"
            + arg.getPosition()
            + "Idx]);");
  }

  private void generateUncheckedArrayCast(
          PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String castName = arg.getTypeName();
    String varName = mkArgumentInternalVarName(arg);
    out.println(
            "    "
                    + arg.getTypeName()
                    + " "
                    + varName
                    + " = ("
                    + castName
                    + ")"
                    + argsArray
                    + "[arg"
                    + arg.getPosition()
                    + "Idx];");
  }

  private void generateCheckedArgumentRead(
      PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String builtinName = capitalize(arg.getTypeName());
    String castName = "TypesGen.expect" + builtinName;
    String varName = mkArgumentInternalVarName(arg);
    out.println("    " + arg.getTypeName() + " " + varName + ";");
    out.println("    try {");
    out.println(
        "      " + varName + " = " + castName + "(" + argsArray + "[arg" + arg.getPosition() + "Idx]);");
    out.println("    } catch (UnexpectedResultException e) {");
    out.println("      com.oracle.truffle.api.CompilerDirectives.transferToInterpreter();");
    out.println("      var builtins = EnsoContext.get(bodyNode).getBuiltins();");
    out.println("      var ensoTypeName = org.enso.interpreter.runtime.type.ConstantsGen.getEnsoTypeName(\"" + builtinName + "\");");
    out.println("      var error = (ensoTypeName != null)");
    out.println("        ? builtins.error().makeTypeError(builtins.fromTypeSystem(ensoTypeName), arguments[arg"
                      + arg.getPosition()
                      + "Idx], \""
                      + varName
                      + "\")");
    out.println("        : builtins.error().makeUnsupportedArgumentsError(new Object[] { arguments[arg"
                      + arg.getPosition()
                      + "Idx] }, \"Unsupported argument for "
                      + varName
                      + " expected a '"
                      + builtinName
                      + "' but got a '\""
                      + " + arguments[arg" + arg.getPosition() + "Idx]"
                      + " + \"' [\""
                      + " + arguments[arg" + arg.getPosition() + "Idx].getClass()"
                      + " + \"]\""
                      + ");");
    out.println("      throw new PanicException(error, bodyNode);");
    out.println("    }");
  }

  private boolean generateWarningsCheck(
      PrintWriter out, List<MethodDefinition.ArgumentDefinition> arguments, String argumentsArray) {
    List<MethodDefinition.ArgumentDefinition> argsToCheck =
        arguments.stream()
            .filter(ArgumentDefinition::shouldCheckWarnings)
            .collect(Collectors.toList());
    if (argsToCheck.isEmpty()) {
      return false;
    } else {
      out.println("    boolean anyWarnings = false;");
      out.println("    ArrayRope<Warning> gatheredWarnings = new ArrayRope<>();");
      for (var arg : argsToCheck) {
        out.println(
            "    if ("
                + arrayRead(argumentsArray, arg.getPosition())
                + " instanceof WithWarnings) {");
        out.println("      internals." + mkArgumentInternalVarName(arg) + WARNING_PROFILE + ".enter();");
        out.println("      anyWarnings = true;");
        out.println(
            "      WithWarnings withWarnings = (WithWarnings) "
                + arrayRead(argumentsArray, arg.getPosition())
                + ";");
        out.println(
            "      "
                + arrayRead(argumentsArray, arg.getPosition())
                + " = withWarnings.getValue();");
        out.println(
            "      gatheredWarnings = gatheredWarnings.prepend(withWarnings.getReassignedWarningsAsRope(bodyNode));");
        out.println("    }");
      }
      return true;
    }
  }

  /**
   * Dumps the information about the collected builtin methods to {@link
   * MethodProcessor#metadataPath()} resource file.
   *
   * <p>The format of a single row in the metadata file: <full name of the method>:<class name of
   * the root node>
   *
   * @param writer a writer to the metadata resource
   * @param pastEntries entries from the previously created metadata file, if any. Entries that
   *     should not be appended to {@code writer} should be removed
   * @throws IOException
   */
  protected void storeMetadata(Writer writer, Map<String, MethodMetadataEntry> pastEntries) throws IOException {
    for (Filer f : builtinMethods.keySet()) {
      for (Map.Entry<String, String[]> entry : builtinMethods.get(f).entrySet()) {
        writer.append(entry.getKey() + ":" + String.join(":", Arrays.asList(entry.getValue())) + "\n");
        if (pastEntries.containsKey(entry.getKey())) {
          pastEntries.remove(entry.getKey());
        }
      }
    }
  }

  protected void registerBuiltinMethod(Filer f, String name, String clazzName, boolean isStatic, boolean isAutoRegister) {
    Map<String, String[]> methods = builtinMethods.get(f);
    if (methods == null) {
      methods = new HashMap<>();
      builtinMethods.put(f, methods);
    }
    methods.put(name, new String[] { clazzName, String.valueOf(isStatic), String.valueOf(isAutoRegister) });
  }

  @Override
  protected String metadataPath() {
    return MethodDefinition.META_PATH;
  }

  protected void cleanup() {
    builtinMethods.clear();
  }

  private String warningCheck(MethodDefinition.ArgumentDefinition arg) {
    return "(" + mkArgumentInternalVarName(arg) + " instanceof WithWarnings)";
  }

  private String mkArgumentInternalVarName(MethodDefinition.ArgumentDefinition arg) {
    return "arg" + arg.getPosition();
  }

  private String arrayRead(String array, int index) {
    return array + "[arg" + index + "Idx]";
  }

  private String capitalize(String name) {
    return name.substring(0, 1).toUpperCase() + name.substring(1);
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  public record MethodMetadataEntry(String fullEnsoName, String clazzName, boolean isStatic, boolean isAutoRegister) implements MetadataEntry {

    @Override
    public String toString() {
      return fullEnsoName + ":" + clazzName + ":" + isStatic + ":" + isAutoRegister;
    }

    @Override
    public String key() {
      return fullEnsoName;
    }
  }

  @Override
  protected MethodMetadataEntry toMetadataEntry(String line) {
    String[] elements = line.split(":");
    if (elements.length != 4) throw new RuntimeException("invalid builtin metadata entry: " + line);
    return new MethodMetadataEntry(elements[0], elements[1], Boolean.parseBoolean(elements[2]), Boolean.parseBoolean(elements[3]));
  }

  private static final String DATAFLOW_ERROR_PROFILE = "IsDataflowErrorConditionProfile";
  private static final String PANIC_SENTINEL_PROFILE = "PanicSentinelBranchProfile";
  private static final String WARNING_PROFILE = "WarningProfile";
}
