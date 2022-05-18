package org.enso.interpreter.dsl;

import com.sun.tools.javac.code.Attribute;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.util.Pair;

import org.apache.commons.lang3.StringUtils;
import org.openide.util.lookup.ServiceProvider;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic;
import javax.tools.FileObject;
import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;

import java.io.*;
import java.lang.annotation.Annotation;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * The processor used to generate code from the methods of the runtime representations annotated
 * with {@link Builtin}.
 *
 * <p>It represents the first stage in generating the final builtin nodes. The processor will
 * generate a corresponding @BuiltinMethod class which, in turn, will get processed by another
 * processor and deliver a RootNode for the method.
 */
@SupportedAnnotationTypes({"org.enso.interpreter.dsl.Builtin", "org.enso.interpreter.dsl.Builtin.Method"})
@ServiceProvider(service = Processor.class)
public class BuiltinsProcessor extends AbstractProcessor {

  private static final String BuiltinsPkg = "org.enso.interpreter.node.expression.builtin";
  private final WrapExceptionExtractor wrapExceptionsExtractor =
          new WrapExceptionExtractor(Builtin.WrapException.class, Builtin.WrapExceptions.class);

  @Override
  public final boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      for (Element elt : annotatedElements) {
        if (elt.getKind() == ElementKind.METHOD || elt.getKind() == ElementKind.CONSTRUCTOR) {
          try {
            handleMethodElement(elt, roundEnv);
          } catch (Exception e) {
            e.printStackTrace();
            processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, e.getMessage());
          }
        } else if (elt.getKind() == ElementKind.CLASS) {
          try {
            handleClassElement(elt, roundEnv);
          } catch (IOException ioe) {
            processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, ioe.getMessage());
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

  public void handleClassElement(Element element, RoundEnvironment roundEnv) throws IOException {
    TypeElement elt = (TypeElement) element;
    Builtin annotation = element.getAnnotation(Builtin.class);
    String clazzName = element.getSimpleName().toString();
    String builtinPkg =
            annotation.pkg().isEmpty() ? BuiltinsPkg : BuiltinsPkg + "." + annotation.pkg();
    ClassName builtinType =  new ClassName(builtinPkg, clazzName);
    JavaFileObject gen =
            processingEnv.getFiler().createSourceFile(builtinType.fullyQualifiedName());
    Optional<String> stdLibName = annotation.stdlibName().isEmpty() ? Optional.empty() : Optional.of(annotation.stdlibName());
    generateBuiltinType(gen, builtinType, stdLibName);
  }

  private void generateBuiltinType(
          JavaFileObject gen,
          ClassName builtinType,
          Optional<String> stdLibName) throws IOException {
    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + builtinType.pkg() + ";");
      out.println();
      for (String importPkg : typeNecessaryImports) {
        out.println("import " + importPkg + ";");
      }
      out.println();
      String stdLib = stdLibName.map(n -> "(name = \"" + n + "\")").orElse("");
      out.println("@BuiltinType" + stdLib);
      out.println("public class " + builtinType.name() + " extends Builtin {}");
      out.println();
    }
  }

  public void handleMethodElement(Element element, RoundEnvironment roundEnv) throws IOException {
    ExecutableElement method = (ExecutableElement) element;
    Element owner = element.getEnclosingElement();

    if (owner.getKind() == ElementKind.CLASS) {
      Builtin ownerAnnotation = owner.getAnnotation(Builtin.class);
      TypeElement tpeElement = (TypeElement) owner;
      PackageElement pkgElement = (PackageElement) tpeElement.getEnclosingElement();
      Builtin.Method annotation = element.getAnnotation(Builtin.Method.class);
      boolean isConstructor = method.getKind() == ElementKind.CONSTRUCTOR;
      String builtinPkg =
              ownerAnnotation.pkg().isEmpty() ? BuiltinsPkg : BuiltinsPkg + "." + ownerAnnotation.pkg();

      SafeWrapException[] wrapExceptions =
              wrapExceptionsExtractor.extract(element);
      Map<String, Integer> parameterCounts = builtinTypesParametersCount(roundEnv);

      if (annotation.expandVarargs() != 0) {
        if (annotation.expandVarargs() < 0) throw new RuntimeException("Invalid varargs value in @Builtin annotation. Must be positive");
        if (!annotation.name().isEmpty()) throw new RuntimeException("Name cannot be non-empty when varargs are used");

        IntStream.rangeClosed(1, annotation.expandVarargs()).forEach(i -> {
          String methodName = (isConstructor ? "new" : method.getSimpleName().toString()) + "_" + i;
          String noUnderscoresMethodName = methodName.replaceAll("_", "");
          String optConstrSuffix = isConstructor ? tpeElement.getSimpleName().toString() : "";
          ClassName builtinMethodNode =
                  new ClassName(
                          builtinPkg,
                          noUnderscoresMethodName.substring(0, 1).toUpperCase() + noUnderscoresMethodName.substring(1) + optConstrSuffix + "Node");
          ClassName ownerClass =
                  new ClassName(pkgElement.getQualifiedName(), tpeElement.getSimpleName());
          try {
            JavaFileObject gen =
                    processingEnv.getFiler().createSourceFile(builtinMethodNode.fullyQualifiedName());
            MethodGenerator methodGen = new MethodGenerator(method, i, wrapExceptions);
            generateBuiltinMethodNode(
                    gen, methodGen, methodName, method.getSimpleName().toString(), annotation.description(), builtinMethodNode, ownerClass, parameterCounts);
          } catch (IOException ioe) {
            throw new RuntimeException(ioe);
          }
        });
      } else {
        String methodName =
                !annotation.name().isEmpty() ? annotation.name() :
                        isConstructor ? "new" : method.getSimpleName().toString();

        String optConstrSuffix = isConstructor ? tpeElement.getSimpleName().toString() : "";
        ClassName builtinMethodNode =
                new ClassName(
                        builtinPkg,
                        methodName.substring(0, 1).toUpperCase() + methodName.substring(1) + optConstrSuffix + "Node");
        ClassName ownerClass =
                new ClassName(pkgElement.getQualifiedName(), tpeElement.getSimpleName());
        JavaFileObject gen =
                processingEnv.getFiler().createSourceFile(builtinMethodNode.fullyQualifiedName());
        MethodGenerator methodGen = new MethodGenerator(method, 0, wrapExceptions);
        generateBuiltinMethodNode(
                gen, methodGen, methodName, method.getSimpleName().toString(), annotation.description(), builtinMethodNode, ownerClass, parameterCounts);
      }
    } else {
      throw new RuntimeException("@Builtin method must be owned by the class");
    }
  }

  /**
   * Returns a map of builtin types and the number of their paramneters.
   * Takes into account the possibility of separate compilation by reading entries from metadate, if any.
   * @param roundEnv current round environment
   * @return a map from a builtin type name to the number of its parameters
   */
  private Map<String, Integer> builtinTypesParametersCount(RoundEnvironment roundEnv) {
    // For separate compilation we need  to read that information from BuiltinTypes metadata file
    Map<String, Integer> pastEntries;
    try {
      FileObject existingFile =
              processingEnv.getFiler().getResource(StandardLocation.CLASS_OUTPUT, "", TypeProcessor.META_PATH);

      try (InputStream resource = existingFile.openInputStream()) {
        pastEntries =
                new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
                        .lines()
                        .map(l -> TypeProcessor.fromStringToMetadataEntry(l))
                        .collect(Collectors.toMap(e -> e.key().replaceAll("_", ""), e -> e.paramNames().length));
      }
    } catch (IOException e) {
      // Ignore, this is a clean run
      pastEntries = new HashMap<>();
    }

    Map<String, Integer> currentRoundEntries =
            roundEnv
            .getElementsAnnotatedWith(BuiltinType.class)
            .stream()
            .collect(Collectors.toMap(e -> e.getSimpleName().toString(), e -> e.getAnnotation(BuiltinType.class).params().length));

    pastEntries.forEach((k, v) -> currentRoundEntries.merge(k, v, (v1, v2) -> v1));

    return currentRoundEntries;
  }

  /**
   * Generates a Java class for @BuiltinMethod Node.
   *
   * @param gen processor's generator
   * @param mgen generator for `execute` method
   * @param methodName target name of the BuiltingMethod (Snake-case)
   * @param ownerMethodName the name of the annotated method
   * @param description short description for the node
   * @param builtinNode class name of the target node
   * @param ownerClazz class name of the owner of the annotated method
   * @param builtinTypesParamCount a map from builtin types to their parameters count
   * @throws IOException throws an exception when we cannot write the new class
   */
  private void generateBuiltinMethodNode(
      JavaFileObject gen,
      MethodGenerator mgen,
      String methodName,
      String ownerMethodName,
      String description,
      ClassName builtinNode,
      ClassName ownerClazz,
      Map<String, Integer> builtinTypesParamCount)
      throws IOException {
    String ensoMethodName = methodName.replaceAll("([^_A-Z])([A-Z])", "$1_$2").toLowerCase();
    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + builtinNode.pkg() + ";");
      out.println();
      for (String importPkg : methodNecessaryImports) {
        out.println("import " + importPkg + ";");
      }
      out.println("import " + ownerClazz.fullyQualifiedName() + ";");
      out.println();
      out.println(
          "@BuiltinMethod(type = \""
              + ownerClazz.name()
              + "\", name = \""
              + ensoMethodName
              + "\", description = \""
              + description
              + "\")");
      out.println("public class " + builtinNode.name() + " extends Node {");
      out.println();

      for (String line : mgen.generateMethod(ownerMethodName, ownerClazz.name(), builtinTypesParamCount)) {
        out.println("  " + line);
      }

      out.println();
      out.println("}");
    }
  }

  private final List<String> typeNecessaryImports =
     Arrays.asList("org.enso.interpreter.dsl.BuiltinType",
                   "org.enso.interpreter.node.expression.builtin.Builtin");
  private final List<String> methodNecessaryImports =
      Arrays.asList(
              "com.oracle.truffle.api.nodes.Node",
              "org.enso.interpreter.dsl.BuiltinMethod",
              "org.enso.interpreter.runtime.builtin.Builtins",
              "org.enso.interpreter.runtime.Context",
              "org.enso.interpreter.runtime.error.PanicException");

  /**
   * Convert annotated method's variable to MethodParameter
   * @param i position of the variable representing the parameter
   * @param v variable element representing the parameter
   * @return MethodParameter encapsulating the method's parameter info
   */
  private MethodParameter fromVariableElementToMethodParameter(int i, VariableElement v) {
    return new MethodParameter(i, v.getSimpleName().toString(), v.asType().toString(),
            v.getAnnotationMirrors().stream().map(am -> am.toString()).collect(Collectors.toList()));
  }

  /** Method generator encapsulates the generation of the `execute` method. */
  private class MethodGenerator {
    private final String returnTpe;
    private final List<MethodParameter> params;
    private final boolean isStatic;
    private final boolean isConstructor;
    private final int varargExpansion;
    private final boolean needsVarargExpansion;
    private final SafeWrapException[] exceptionWrappers;

    public MethodGenerator(ExecutableElement method, int expandedVarargs, SafeWrapException[] exceptionWrappers) {
      this(method, method.getParameters(), expandedVarargs, exceptionWrappers);
    }

    private MethodGenerator(ExecutableElement method, List<? extends VariableElement> params, int expandedVarargs, SafeWrapException[] exceptionWrappers) {
      this(
          method.getReturnType().toString(),
          IntStream.range(0, method.getParameters().size()).mapToObj(i ->
                  fromVariableElementToMethodParameter(i, params.get(i))).collect(Collectors.toList()),
          method.getModifiers().contains(Modifier.STATIC),
          method.getKind() == ElementKind.CONSTRUCTOR,
          expandedVarargs,
          method.isVarArgs(),
          exceptionWrappers);
    }

    private MethodGenerator(
            String returnTpe,
            List<MethodParameter> params,
            boolean isStatic,
            boolean isConstructor,
            int expandedVarargs,
            boolean isVarargs,
            SafeWrapException[] exceptionWrappers) {
      this.returnTpe = returnTpe;
      this.params = params;
      this.isStatic = isStatic;
      this.isConstructor = isConstructor;
      this.varargExpansion = expandedVarargs;
      this.needsVarargExpansion = isVarargs && (varargExpansion > 0);
      this.exceptionWrappers = exceptionWrappers;
    }

    private Optional<Integer> expandVararg(int paramIndex) {
      return needsVarargExpansion && params.size() >= (paramIndex + 1) ? Optional.of(varargExpansion) : Optional.empty();
    }

    private String fromAnnotationValueToClassName(Attribute.Class clazz) {
      String[] clazzElements = clazz.classType.baseType().toString().split("\\.");
      if (clazzElements.length == 0) {
        return clazz.classType.baseType().toString();
      } else {
        return clazzElements[clazzElements.length - 1];
      }
    }

    public List<String> generateMethod(String name, String owner, Map<String, Integer> builtinTypesParameterCounts) {
      String paramsDef;
      String paramsApplied;
      if (params.isEmpty()) {
        paramsDef = "";
        paramsApplied = "";
      } else {
        paramsDef =
            ", "
                + StringUtils.join(params.stream().flatMap(x -> x.declaredParameters(expandVararg(x.index))).toArray(), ", ");
        paramsApplied =
            StringUtils.join(params.stream().flatMap(x -> x.names(expandVararg(x.index))).toArray(), ", ");
      }
      String thisParamTpe = isStatic || isConstructor ? "Object" : owner;
      String targetReturnTpe = isConstructor ? "Object" : returnTpe;
      String body;
      if (isConstructor) {
        body = "  return new " + owner + "(" + paramsApplied + ");";
      } else {
        body = isStatic
                ? "  return " + owner + "." + name + "(" + paramsApplied + ");"
                : "  return _this." + name + "(" + paramsApplied + ");";
      }
      boolean wrapsExceptions = exceptionWrappers.length != 0;
      if (wrapsExceptions) {
        List<String> wrappedBody = new ArrayList<>();
        wrappedBody.add(targetReturnTpe + " execute(" + thisParamTpe + " _this" + paramsDef + ") {");
        wrappedBody.add("  try {");
        wrappedBody.add("  " + body);
        for (int i=0; i < exceptionWrappers.length; i++) {
          wrappedBody.addAll(exceptionWrappers[i].toCatchClause(params, builtinTypesParameterCounts));
        }
        wrappedBody.add("  }");
        wrappedBody.add("}");
        return wrappedBody;
      } else {
        return List.of(
                targetReturnTpe + " execute(" + thisParamTpe + " _this" + paramsDef + ") {",
                body,
                "}"
        );
      }
    }

  }


  private record ClassName(String pkg, String name) {
    private ClassName(Name pkgName, Name nameSeq) {
      this(pkgName.toString(), nameSeq.toString());
    }

    public String fullyQualifiedName() {
      return pkg + "." + name;
    }
  }

  /**
   * MethodParameter encapsulates the generation of string representation of the parameter.
   * Additionally, it can optionally expand vararg parameters.
   */
  private record MethodParameter(int index, String name, String tpe, List<String> annotations) {
    /**
     * Returns a parameter's declaration.
     * If the parameter represents a vararg, the declaration is repeated. Otherwise return a single
     * element Stream of declarations.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter, potentially repeated for varargs
     */
    public Stream<String> declaredParameters(Optional<Integer> expand) {
      // If the parameter is the expanded vararg we must get rid of the `[]` suffix
      String parameterTpe = expand.isEmpty() ? tpe : tpe.substring(0, tpe.length() - 2);
      String copiedAnnotations = annotations.isEmpty() ? "" : (StringUtils.joinWith(" ", annotations.toArray()) + " ");
      return names(expand).map(n -> copiedAnnotations + parameterTpe + " " + n);
    }

    /**
     * Returns a parameter's name..
     * If the parameter represents a vararg, the name is repeated. Otherwise return a single
     * element Stream of declarations.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter, potentially repeated for varargs
     */
    public Stream<String> names(Optional<Integer> expand) {
      return expand.map(e->
        IntStream.range(0, e).mapToObj(i-> name + "_" + (i+1))
      ).orElse(Stream.of(name));
    }
  }

  /**
   * Wrapper around {@link Builtin.WrapException} annotation with all elements of Class type resolved.
   * extracted
   */
  private record SafeWrapException(Attribute.Class from, Attribute.Class to) {

    /**
     * Generate a catch-clause that catches `from`, wraps it into `to` Enso type and rethrows the latter
     * @param methodParameters list of all method's parameters, potentially to be applied to `to` constructor
     * @param builtinTypesParameterCounts a map from builtin errors to the number of parameters in their constructors
     * @return Lines representing the (unclosed) catch-clause catching the runtime `from` exception
     */
    List<String> toCatchClause(List<MethodParameter> methodParameters, Map<String, Integer> builtinTypesParameterCounts) {
      String from = fromAttributeToClassName(from());
      String to = fromAttributeToClassName(to());
      int toParamCount = errorParametersCount(to(), builtinTypesParameterCounts);
      List<String> errorParameters =
              methodParameters
                      .stream()
                      .limit(toParamCount - 1)
                      .flatMap(x -> x.names(Optional.empty()))
                      .collect(Collectors.toList());
      String errorParameterCode = errorParameters.isEmpty() ? "" : ", " + StringUtils.join(errorParameters, ", ");
      return List.of(
              "  } catch (" + from + " exception) {",
              "    Builtins builtins = Context.get(this).getBuiltins();",
              "    throw new PanicException(builtins.error().make" + to + "(_this" + errorParameterCode + "), this);"
      );
    }

    private String fromAttributeToClassName(Attribute.Class clazz) {
      String[] clazzElements = clazz.classType.baseType().toString().split("\\.");
      if (clazzElements.length == 0) {
        return clazz.classType.baseType().toString();
      } else {
        return clazzElements[clazzElements.length - 1];
      }
    }

    private int errorParametersCount(Attribute.Class clazz, Map<String, Integer> builtinTypesParameterCounts) {
      String clazzSimple = fromAttributeToClassName(clazz);
      // `this` counts as 1
      return builtinTypesParameterCounts.getOrDefault(clazzSimple, 1);
    }
  }

  /**
   * Helper class that encapsulates retrieving the values of elements which type involves Class<?>.
   * Such elements' values cannot be retrieved by invoking the <element>() method. Instead one has to go through mirrors.
   * The logic has to deal with the following scenarios:
   * - method with an individual annotation
   * - method with multiple annotations of the same type, thus implicitly being annotation with
   *   container annotation
   * - method with an explicit container annotation
   *
   * Refer to <a href="https://area-51.blog/2009/02/13/getting-class-values-from-annotations-in-an-annotationprocessor/">blog</a>
   * for details.
   **/
  private class WrapExceptionExtractor {

    private static final String FromElementName = "from";
    private static final String ToElementName = "to";
    private static final String ValueElementName = "value";

    private Class<? extends Annotation> wrapExceptionAnnotationClass;
    private Class<? extends Annotation> wrapExceptionsAnnotationClass;

    public WrapExceptionExtractor(
            Class<? extends Annotation> wrapExceptionAnnotationClass,
            Class<? extends Annotation> wrapExceptionsAnnotationClass) {
      this.wrapExceptionAnnotationClass = wrapExceptionAnnotationClass;
      this.wrapExceptionsAnnotationClass = wrapExceptionsAnnotationClass;
    }

    /**
     * Extract {@link org.enso.interpreter.dsl.Builtin.WrapException} from the annotated element
     * in a mirror-safe manner.
     *
     * @param element a method annotated with either {@link org.enso.interpreter.dsl.Builtin.WrapException} or
     *                {@link org.enso.interpreter.dsl.Builtin.WrapExceptions}
     * @return An array of safely retrieved (potentially repeated) values of
     *         {@link org.enso.interpreter.dsl.Builtin.WrapException} annotation(s)
     */
    public SafeWrapException[] extract(Element element) {
      if (element.getAnnotation(wrapExceptionsAnnotationClass) != null) {
        return extractClassElementFromAnnotationContainer(element, wrapExceptionsAnnotationClass);
      } else if (element.getAnnotation(wrapExceptionAnnotationClass) != null) {
        return extractClassElementFromAnnotation(element, wrapExceptionAnnotationClass);
      } else {
        return new SafeWrapException[0];
      }
    }

    private SafeWrapException[] extractClassElementFromAnnotation(Element element, Class<?> annotationClass) {
      Element builtinElement = processingEnv.getElementUtils().getTypeElement(annotationClass.getCanonicalName());
      TypeMirror builtinType = builtinElement.asType();

      List<SafeWrapException> exceptionWrappers = new ArrayList<>();
      for (AnnotationMirror am: element.getAnnotationMirrors()) {
        if (am.getAnnotationType().equals(builtinType)) {
          Attribute.Class valueFrom = null;
          Attribute.Class valueTo = null;
          for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry : am.getElementValues().entrySet() ) {
            if (FromElementName.equals(entry.getKey().getSimpleName().toString())) {
              valueFrom = (Attribute.Class)(entry.getValue());
            } else if (ToElementName.equals(entry.getKey().getSimpleName().toString())) {
              valueTo = (Attribute.Class)(entry.getValue());
            }
          }
          if (valueFrom != null && valueTo != null) {
            exceptionWrappers.add(new SafeWrapException(valueFrom, valueTo));
          }
        }
      }
      return exceptionWrappers.toArray(new SafeWrapException[0]);
    }


    private SafeWrapException[] extractClassElementFromAnnotationContainer(Element element, Class<?> annotationClass) {

      Element builtinElement = processingEnv.getElementUtils().getTypeElement(annotationClass.getCanonicalName());
      Types tpeUtils = processingEnv.getTypeUtils();
      TypeMirror builtinType = builtinElement.asType();

      List<SafeWrapException> wrappedExceptions = new ArrayList<>();
      for (AnnotationMirror am: element.getAnnotationMirrors()) {
        if (tpeUtils.isSameType(am.getAnnotationType(), builtinType)) {
          for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry : am.getElementValues().entrySet()) {
            if (ValueElementName.equals(entry.getKey().getSimpleName().toString())) {
              Attribute.Array wrapExceptions = (Attribute.Array)entry.getValue();
              for (int i = 0; i<wrapExceptions.values.length; i++) {
                Attribute.Class valueFrom = null;
                Attribute.Class valueTo = null;
                Attribute.Compound attr = (Attribute.Compound)wrapExceptions.values[i];
                for (Pair<Symbol.MethodSymbol, Attribute> p: attr.values) {
                  if (p.fst.getSimpleName().contentEquals(FromElementName)) {
                    valueFrom = (Attribute.Class)p.snd;
                  } else if (p.fst.getSimpleName().contentEquals(ToElementName)) {
                    valueTo = (Attribute.Class)p.snd;
                  }
                }
                if (valueFrom != null && valueTo != null) {
                  wrappedExceptions.add(new SafeWrapException(valueFrom, valueTo));
                }
              }
              break;
            }
          }
        }
      }
      return wrappedExceptions.toArray(new SafeWrapException[0]);
    }

  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }
}
