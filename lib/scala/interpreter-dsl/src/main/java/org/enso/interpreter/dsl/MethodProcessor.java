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
public class MethodProcessor
    extends BuiltinsMetadataProcessor<MethodProcessor.MethodMetadataEntry> {

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
            handleTypeElement((TypeElement) elt, needsFrame);
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

  private void handleTypeElement(TypeElement element, Boolean needsFrame) throws IOException {
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
    registerBuiltinMethod(
        processingEnv.getFiler(),
        def.getDeclaredName(),
        fullClassName,
        def.isStatic(),
        def.isAutoRegister());
    if (def.hasAliases()) {
      for (String alias : def.aliases()) {
        registerBuiltinMethod(
            processingEnv.getFiler(), alias, fullClassName, def.isStatic(), def.isAutoRegister());
      }
    }
  }

  private final List<String> necessaryImports =
      Arrays.asList(
          "com.oracle.truffle.api.CompilerDirectives",
          "com.oracle.truffle.api.dsl.UnsupportedSpecializationException",
          "com.oracle.truffle.api.frame.VirtualFrame",
          "com.oracle.truffle.api.interop.InteropLibrary",
          "com.oracle.truffle.api.interop.UnsupportedMessageException",
          "com.oracle.truffle.api.nodes.ControlFlowException",
          "com.oracle.truffle.api.nodes.Node",
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
          "org.enso.interpreter.runtime.builtin.Builtins",
          "org.enso.interpreter.runtime.data.hash.EnsoHashMap",
          "org.enso.interpreter.runtime.data.hash.HashMapInsertNode",
          "org.enso.interpreter.runtime.data.hash.HashMapInsertAllNode",
          "org.enso.interpreter.runtime.data.text.Text",
          "org.enso.interpreter.runtime.error.DataflowError",
          "org.enso.interpreter.runtime.error.PanicException",
          "org.enso.interpreter.runtime.state.State",
          "org.enso.interpreter.runtime.type.TypesGen",
          "org.enso.interpreter.runtime.warning.Warning",
          "org.enso.interpreter.runtime.warning.WarningsLibrary",
          "org.enso.interpreter.runtime.warning.WithWarnings",
          "org.enso.interpreter.runtime.warning.AppendWarningNode");

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
        out.println(
            "public class " + methodDefinition.getClassName() + " extends BuiltinRootNode {");
      } else {
        out.println(
            "public class "
                + methodDefinition.getClassName()
                + " extends BuiltinRootNode implements InlineableNode.Root {");
      }
      out.println("  private @Child " + methodDefinition.getOriginalClassName() + " bodyNode;");
      out.println("  private @Children ArgNode[] argNodes = new ArgNode[] {");
      generateArguments(methodDefinition, out);
      out.println("    };");
      out.println("  private static final class Internals {");
      out.println("    Internals(boolean s) {");
      out.println("      this.staticOrInstanceMethod = s;");
      out.println("    }");
      out.println();
      out.println("    private final boolean staticOrInstanceMethod;");
      out.println("  }");
      out.println("  private final Internals internals;");

      out.println(
          "  private "
              + methodDefinition.getClassName()
              + "(EnsoLanguage language, boolean staticOrInstanceMethod) {");
      out.println("    super(language);");
      out.println("    this.bodyNode = " + methodDefinition.getConstructorExpression() + ";");
      out.println("    this.internals = new Internals(staticOrInstanceMethod);");
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
      out.println(
          "  public static Function makeFunction(EnsoLanguage language, boolean"
              + " staticOrInstanceMethod) {");
      out.println("    if (staticOrInstanceMethod) {");
      out.println("      return Function." + functionBuilderMethod + "(");
      out.print(
          "        new " + methodDefinition.getClassName() + "(language, staticOrInstanceMethod)");
      List<String> argsStaticInstace =
          generateMakeFunctionArgs(true, methodDefinition.getArguments());
      if (!argsStaticInstace.isEmpty()) {
        out.println(",");
      }
      out.println(String.join(",\n", argsStaticInstace) + ");");
      out.println("    } else {");
      out.println("      return Function." + functionBuilderMethod + "(");
      out.print(
          "        new " + methodDefinition.getClassName() + "(language, staticOrInstanceMethod)");
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
        out.println(
            "      private final Internals extra = new"
                + " Internals(internals.staticOrInstanceMethod);");
        out.println(
            "      private @Child "
                + methodDefinition.getOriginalClassName()
                + " body = "
                + methodDefinition.getConstructorExpression()
                + ";");
        out.println("      private @Children ArgNode[] argNodes = new ArgNode[] {");
        generateArguments(methodDefinition, out);
        out.println("      };");
        out.println("      @Override");
        out.println("      public Object call(VirtualFrame frame, Object[] args) {");
        out.println("        return handleExecute(argNodes, frame, extra, body, args);");
        out.println("      }");
        out.println("    }");
        out.println();
        out.println("    return new Inlineable();");
        out.println("  }");
      }

      out.println();

      out.println("  @Override");
      out.println("  public Object execute(VirtualFrame frame) {");
      if (methodDefinition.needsFrame()) {
        out.println("    var args = frame.getArguments();");
      } else {
        out.println(
            "    return handleExecute(argNodes, frame, this.internals, bodyNode,"
                + " frame.getArguments());");
        out.println("  }");
        out.println(
            "  private static Object handleExecute(ArgNode[] argNodes, VirtualFrame frame,"
                + " Internals internals, "
                + methodDefinition.getOriginalClassName()
                + " bodyNode, Object[] args) {");
      }
      out.println("    var prefix = internals.staticOrInstanceMethod ? 1 : 0;");
      out.println("    State state = Function.ArgumentsHelper.getState(args);");
      if (methodDefinition.needsCallerInfo()) {
        out.println("    CallerInfo callerInfo = Function.ArgumentsHelper.getCallerInfo(args);");
      }
      out.println(
          "    Object[] arguments = Function.ArgumentsHelper.getPositionalArguments(args);");
      List<String> callArgNames = new ArrayList<>();
      for (MethodDefinition.ArgumentDefinition arg : methodDefinition.getArguments()) {
        if (!(arg.isImplicit()
            || arg.isFrame()
            || arg.isState()
            || arg.isCallerInfo()
            || arg.isNode())) {
          out.println(
              "    int arg" + arg.getPosition() + "Idx = " + arg.getPosition() + " + prefix;");
        }
      }
      out.println("    var argCtx = new ArgContext();");
      boolean warningsPossible =
          methodDefinition.getArguments().stream()
                  .filter(ArgumentDefinition::shouldCheckWarnings)
                  .count()
              != 0;
      for (MethodDefinition.ArgumentDefinition ad : methodDefinition.getArguments()) {
        if (ad.isImplicit()) {
        } else if (ad.isState()) {
          callArgNames.add("state");
        } else if (ad.isFrame()) {
          callArgNames.add("frame");
        } else if (ad.isNode()) {
          callArgNames.add("bodyNode");
        } else if (ad.isCallerInfo()) {
          callArgNames.add("callerInfo");
        } else {
          var plain = ad.getTypeName();
          var boxed = wrapperTypeName(ad);
          if (plain.equals(boxed)) {
            callArgNames.add(mkArgumentInternalVarName(ad));
          } else {
            callArgNames.add("(" + plain + ")" + mkArgumentInternalVarName(ad));
          }
          var argReference = "arguments[arg" + ad.getPosition() + "Idx]";
          var varName = mkArgumentInternalVarName(ad);
          out.println(
              "    var "
                  + varName
                  + " = argNodes["
                  + ad.getPosition()
                  + "].processArgument(frame, "
                  + boxed
                  + ".class, "
                  + argReference
                  + ", argCtx);");
        }
      }
      out.println("    if (argCtx.getReturnValue() != null) return argCtx.getReturnValue();");
      out.println("    Object result;");
      var executeCall = "bodyNode.execute(" + String.join(", ", callArgNames) + ")";
      out.println(wrapInTryCatch("result = " + executeCall + ";", 4));
      if (warningsPossible) {
        out.println("    if (argCtx.hasWarnings()) {");
        out.println("      return argNodes[0].processWarnings(frame, result, argCtx);");
        out.println("    } else {");
        out.println("      return result;");
        out.println("    }");
      } else {
        out.println("    return result;");
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
      out.println(
          "    return new "
              + methodDefinition.getClassName()
              + "(EnsoLanguage.get(this), internals.staticOrInstanceMethod);");
      out.println("  }");

      out.println();

      out.println("}");
    }
  }

  private static void generateArguments(MethodDefinition methodDefinition, final PrintWriter out) {
    for (MethodDefinition.ArgumentDefinition arg : methodDefinition.getArguments()) {
      if (!arg.isPositional()) {
        continue;
      }
      var checkErrors = arg.shouldCheckErrors();
      var checkPanicSentinel = arg.isPositional() && !arg.isSelf();
      var checkWarnings = arg.shouldCheckWarnings();
      if (arg.isArray()) {
        out.println("/* array argument is not supported for " + arg.getName() + "*/");
        continue;
      }
      out.println(
          "        ArgNode.create("
              + arg.isSelf()
              + ", "
              + arg.requiresCast()
              + ", "
              + checkErrors
              + ", "
              + checkPanicSentinel
              + ", "
              + checkWarnings
              + "),");
    }
  }

  private String wrapInTryCatch(String statement, int indent) {
    var indentStr = " ".repeat(indent);
    var sb = new StringBuilder();
    sb.append(indentStr).append("try {").append("\n");
    sb.append(indentStr).append("  ").append(statement).append("\n");
    sb.append(indentStr)
        .append("} catch (UnsupportedSpecializationException unsupSpecEx) {")
        .append("\n");
    sb.append(indentStr)
        .append("  CompilerDirectives.transferToInterpreterAndInvalidate();")
        .append("\n");
    sb.append(indentStr)
        .append("  Builtins builtins = EnsoContext.get(bodyNode).getBuiltins();")
        .append("\n");
    sb.append(indentStr)
        .append(
            "  var unimplErr = builtins.error().makeUnimplemented(\"Unsupported specialization: \""
                + " + unsupSpecEx.getMessage());")
        .append("\n");
    sb.append(indentStr).append("  throw new PanicException(unimplErr, bodyNode);").append("\n");
    sb.append(indentStr).append("}").append("\n");
    return sb.toString();
  }

  private List<String> generateMakeFunctionArgs(
      boolean staticInstance, List<ArgumentDefinition> args) {
    List<String> argumentDefs = new ArrayList<>();
    int staticPrefix = 0;
    if (staticInstance) {
      argumentDefs.add(
          "        new ArgumentDefinition(0, \"selfStatic\", null, null,"
              + " ArgumentDefinition.ExecutionMode.EXECUTE)");
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

  private String wrapperTypeName(MethodDefinition.ArgumentDefinition arg) {
    var tn = capitalize(arg.getTypeName());
    if ("Boolean".equals(tn)) {
      return "java.lang.Boolean";
    } else {
      return tn;
    }
  }

  /**
   * Dumps the information about the collected builtin methods to {@link
   * MethodProcessor#metadataPath()} resource file.
   *
   * <p>The format of a single row in the metadata file:
   *
   * <pre>
   * "full name of the method":"class name of the root node"
   * </pre>
   *
   * @param writer a writer to the metadata resource
   * @param pastEntries entries from the previously created metadata file, if any. Entries that
   *     should not be appended to {@code writer} should be removed
   * @throws IOException
   */
  @Override
  protected void storeMetadata(Writer writer, Map<String, MethodMetadataEntry> pastEntries)
      throws IOException {
    for (Filer f : builtinMethods.keySet()) {
      for (Map.Entry<String, String[]> entry : builtinMethods.get(f).entrySet()) {
        writer.append(
            entry.getKey() + ":" + String.join(":", Arrays.asList(entry.getValue())) + "\n");
        if (pastEntries.containsKey(entry.getKey())) {
          pastEntries.remove(entry.getKey());
        }
      }
    }
  }

  protected void registerBuiltinMethod(
      Filer f, String name, String clazzName, boolean isStatic, boolean isAutoRegister) {
    Map<String, String[]> methods = builtinMethods.get(f);
    if (methods == null) {
      methods = new HashMap<>();
      builtinMethods.put(f, methods);
    }
    methods.put(
        name, new String[] {clazzName, String.valueOf(isStatic), String.valueOf(isAutoRegister)});
  }

  @Override
  protected String metadataPath() {
    return MethodDefinition.META_PATH;
  }

  protected void cleanup() {
    builtinMethods.clear();
  }

  private String mkArgumentInternalVarName(MethodDefinition.ArgumentDefinition arg) {
    return "arg" + arg.getPosition();
  }

  private String capitalize(String name) {
    return name.substring(0, 1).toUpperCase() + name.substring(1);
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  public record MethodMetadataEntry(
      String fullEnsoName, String clazzName, boolean isStatic, boolean isAutoRegister)
      implements MetadataEntry {

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
    return new MethodMetadataEntry(
        elements[0],
        elements[1],
        Boolean.parseBoolean(elements[2]),
        Boolean.parseBoolean(elements[3]));
  }
}
