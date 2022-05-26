package org.enso.interpreter.dsl;

import com.sun.tools.javac.code.Attribute;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.util.Pair;

import com.google.common.base.CaseFormat;
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
  private static final WrapExceptionExtractor wrapExceptionsExtractor =
          new WrapExceptionExtractor(Builtin.WrapException.class, Builtin.WrapExceptions.class);

  private record Specialized(String owner, String methodName) {}
  private Map<Specialized, List<ExecutableElement>> specializedMethods = new HashMap<>();


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

  /**
   * Generate internal @BuiltinType class used by the compiler.
   *
   * @param element class annotated with @Builtin
   * @param roundEnv meta information about the current round of processing
   */
  public void handleClassElement(Element element, RoundEnvironment roundEnv) throws IOException {
    TypeElement elt = (TypeElement) element;
    Builtin annotation = element.getAnnotation(Builtin.class);
    String clazzName = annotation.name().isEmpty() ? element.getSimpleName().toString() : annotation.name();
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

  /**
   * Generate m @BuiltinMethod node class
   *
   * @param element method annotated with @Builtin.Method which implementation and signature
   *                will be used to generate the target node class
   * @param roundEnv meta information about the current round of processing
   */
  public void handleMethodElement(Element element, RoundEnvironment roundEnv) throws IOException {
    ExecutableElement method = (ExecutableElement) element;
    Element owner = element.getEnclosingElement();

    if (owner.getKind() == ElementKind.CLASS) {
      Builtin ownerAnnotation = owner.getAnnotation(Builtin.class);
      assert(ownerAnnotation != null);
      TypeElement ownerTpeElement = (TypeElement) owner;
      String ownerName = ownerAnnotation.name().isEmpty() ? ownerTpeElement.getSimpleName().toString() : ownerAnnotation.name();
      PackageElement pkgElement = (PackageElement) ownerTpeElement.getEnclosingElement();

      Builtin.Method annotation = element.getAnnotation(Builtin.Method.class);
      boolean isConstructor = method.getKind() == ElementKind.CONSTRUCTOR;
      String builtinPkg =
              ownerAnnotation.pkg().isEmpty() ? BuiltinsPkg : BuiltinsPkg + "." + ownerAnnotation.pkg();

      Map<String, Integer> parameterCounts = builtinTypesParametersCount(roundEnv);

      if (annotation.expandVarargs() != 0) {
        if (annotation.expandVarargs() < 0) throw new RuntimeException("Invalid varargs value in @Builtin annotation. Must be positive");
        if (!annotation.name().isEmpty()) throw new RuntimeException("Name cannot be non-empty when varargs are used");

        IntStream.rangeClosed(1, annotation.expandVarargs()).forEach(i -> {
          String methodName = (isConstructor ? "new" : method.getSimpleName().toString()) + "_" + i;
          String clazzName = CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, methodName);
          String optConstrSuffix = isConstructor ? ownerName : "";
          ClassName builtinMethodNode =
                  new ClassName(
                          builtinPkg,
                          clazzName + optConstrSuffix + "Node");
          ClassName ownerClass =
                  new ClassName(pkgElement.getQualifiedName().toString(), ownerTpeElement.getSimpleName().toString());
          ClassName stdLibOwnerClass =
                  new ClassName(pkgElement.getQualifiedName().toString(), ownerName);

          try {
            MethodNodeClassGenerator classGenerator =
                    new NoSpecializationClassGenerator(method, builtinMethodNode, ownerClass, stdLibOwnerClass, i, parameterCounts);
            classGenerator.generate(methodName, annotation.description(), method.getSimpleName().toString());
          } catch (IOException ioe) {
            throw new RuntimeException(ioe);
          }
        });
      } else {
        String builtinMethodName =
          !annotation.name().isEmpty() ? annotation.name() :
                          isConstructor ? "new" : method.getSimpleName().toString();
        String builtinMethodNameClass =
                !annotation.name().isEmpty() ? CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, annotation.name()) :
                        isConstructor ? "new" : method.getSimpleName().toString();
        Builtin.Specialize specialize = element.getAnnotation(Builtin.Specialize.class);



        String optConstrSuffix = isConstructor ? ownerName : "";
        ClassName builtinMethodNode =
                new ClassName(
                        builtinPkg,
                        CaseFormat.LOWER_CAMEL.to(CaseFormat.UPPER_CAMEL, builtinMethodNameClass) + optConstrSuffix + "Node");
        ClassName ownerClass =
                new ClassName(pkgElement.getQualifiedName().toString(), ownerTpeElement.getSimpleName().toString());
        ClassName stdLibOwnerClass =
                new ClassName(pkgElement.getQualifiedName().toString(), ownerName);

        int expected = specializationsCount(owner, builtinMethodName);
        if (specialize != null) {
          Specialized key = new Specialized(ownerName, builtinMethodName);
          List<ExecutableElement> encountered = specializedMethods.compute(key, (k, v) -> {
            if (v == null) {
              List<ExecutableElement> elements = new ArrayList<>();
              elements.add(method);
              return elements;
            } else {
              v.add(method);
              return v;
            }
          });
          if (encountered.size() == expected) {
            MethodNodeClassGenerator classGenerator =
                    new SpecializationClassGenerator(encountered, builtinMethodNode, ownerClass, stdLibOwnerClass, parameterCounts);
            classGenerator.generate(builtinMethodName, annotation.description(), method.getSimpleName().toString());
          } else {
            return;
          }
        } else {

          MethodNodeClassGenerator classGenerator =
                  new NoSpecializationClassGenerator(method, builtinMethodNode, ownerClass, stdLibOwnerClass, parameterCounts);
          classGenerator.generate(builtinMethodName, annotation.description(), method.getSimpleName().toString());
        }
      }
    } else {
      throw new RuntimeException("@Builtin method must be owned by the class");
    }
  }

  /**
   * Count the number of @Specialization or @Fallback annotations for a given method name.
   *
   * @param owner @Builtin class
   * @param builtinMethodName Enso-name of the builtin method that is overloaded
   * @return number of expected specializations during annotation processing for the given method
   */
  private int specializationsCount(Element owner, String builtinMethodName) {
    return (int) owner.getEnclosedElements().stream().filter(e -> {
      if (e.getKind() != ElementKind.METHOD &&  e.getKind() != ElementKind.CONSTRUCTOR) return false;
      Builtin.Method annotation = e.getAnnotation(Builtin.Method.class);
      Builtin.Specialize specializedAnnot = e.getAnnotation(Builtin.Specialize.class);
      if (annotation == null || specializedAnnot == null) return false;
      boolean isConstructor = e.getKind() == ElementKind.CONSTRUCTOR;
      String name =
              !annotation.name().isEmpty() ? annotation.name() :
                      isConstructor ? "new" : e.getSimpleName().toString();
      return name.equals(builtinMethodName);
    }).count();
  }

  /**
   * Returns a map of builtin types and the number of their parameters.
   * Takes into account the possibility of separate compilation by reading entries from metadate, if any.
   * The map is used to automatically generate try/catch for the possible exceptions.
   *
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


  private abstract class MethodNodeClassGenerator {
    ClassName builtinNode;
    ClassName ownerClazz;
    ClassName stdlibOwner;

    public MethodNodeClassGenerator(ClassName builtinNode, ClassName ownerClazz, ClassName stdlibOwner) {
      this.builtinNode = builtinNode;
      this.ownerClazz = ownerClazz;
      this.stdlibOwner = stdlibOwner;
    }

    protected SafeWrapException[] wrapExceptions(Element element) {
      return wrapExceptionsExtractor.extract(processingEnv, element);
    }

    /**
     * Checks if the method has been marked require explicit to guest value translations
     *
     * @param origin builtin method
     * @return true if the annotation exists, false otherwise
     */
    protected boolean needsGuestValueConversion(Element origin) {
      return origin.getAnnotation(Builtin.ReturningGuestObject.class) != null;
    }


    public void generate(String methodName, String description, String ownerMethodName) throws IOException {
      JavaFileObject gen = processingEnv.getFiler().createSourceFile(builtinNode.fullyQualifiedName());;
      try (PrintWriter out = new PrintWriter(gen.openWriter())) {
        generateClassHeader(out, methodName, description);
        generateClass(out, methodName, ownerMethodName);
      }
    }

    /**
     * Generate full class definition.
     *
     * @param out output stream where code will be written to
     * @param methodName name of the annotated method (lower camel-case)
     * @param ownerMethodName
     * @throws IOException
     */
    protected abstract void generateClass(PrintWriter out, String methodName, String ownerMethodName) throws IOException;

    /**
     * Generate package, imports and @BuiltinMethod for a target class
     *
     * @param out output stream where code will be written to
     * @param methodName name of the annotated method (lower camel-case)
     * @param description description of the builtin method
     */
    private void generateClassHeader(PrintWriter out, String methodName, String description){
      String ensoMethodName = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, methodName);
      String ensoTypeName = stdlibOwner.name().replaceAll("([a-z])([A-Z])", "$1_$2");
      out.println("package " + builtinNode.pkg() + ";");
      out.println();
      for (String importPkg : methodNecessaryImports) {
        out.println("import " + importPkg + ";");
      }
      out.println("import " + ownerClazz.fullyQualifiedName() + ";");
      out.println();
      out.println(
              "@BuiltinMethod(type = \""
                      + ensoTypeName
                      + "\", name = \""
                      + ensoMethodName
                      + "\", description = \""
                      + description
                      + "\")");
    }

  }

  /**
   * Generator for a builtin method with specializations.
   * The target class will always be abstract and constructed via a static `build` method pattern.
   * The generator will also infer parameters to specialize on and generate methods for them.
   */
  private class SpecializationClassGenerator extends MethodNodeClassGenerator {
    ClassName builtinNode;
    ClassName ownerClazz;
    ClassName stdlibOwner;
    Map<String, Integer> builtinTypesParamCount;
    List<ExecutableElement> elements;

    public SpecializationClassGenerator(List<ExecutableElement> methodElements, ClassName builtinNode, ClassName ownerClazz, ClassName stdlibOwner, Map<String, Integer> parameterCounts) {
      super(builtinNode, ownerClazz, stdlibOwner);
      this.builtinNode = builtinNode;
      this.ownerClazz = ownerClazz;
      this.stdlibOwner = stdlibOwner;
      this.builtinTypesParamCount = parameterCounts;
      this.elements = methodElements;
    }

    @Override
    public void generateClass(PrintWriter out, String methodName, String ownerMethodName) {
      MethodGenerator mgen = new SpecializedMethodsGenerator(elements);
      out.println("public abstract class " + builtinNode.name() + " extends Node {");
      out.println();

      out.println("  static " + builtinNode.name() + " build() {");
      out.println("    return " + builtinNode.name() + "Gen.create();");
      out.println("  }");

      for (String line : mgen.generate(ownerMethodName, ownerClazz.name(), builtinTypesParamCount)) {
        out.println("  " + line);
      }

      out.println();
      out.println("}");
    }
  }

  /**
   * Generator for builtin method class with no specialization.
   */
  private class NoSpecializationClassGenerator extends MethodNodeClassGenerator {

    ClassName builtinNode;
    ClassName ownerClazz;
    ExecutableElement origin;
    Map<String, Integer> builtinTypesParamCount;
    int varArgExpansion;

    public NoSpecializationClassGenerator(
            ExecutableElement origin,
            ClassName builtinNode,
            ClassName ownerClazz,
            ClassName stdlibOwner,
            int varArgExpansion,
            Map<String, Integer> builtinTypesParamCount) {
      super(builtinNode, ownerClazz, stdlibOwner);
      this.builtinNode = builtinNode;
      this.ownerClazz = ownerClazz;
      this.origin = origin;
      this.builtinTypesParamCount = builtinTypesParamCount;
      this.varArgExpansion = varArgExpansion;
    }

    public NoSpecializationClassGenerator(
            ExecutableElement origin,
            ClassName builtinNode,
            ClassName ownerClazz,
            ClassName stdlibOwner,
            Map<String, Integer> builtinTypesParamCount) {
      this(origin, builtinNode, ownerClazz, stdlibOwner, 0, builtinTypesParamCount);
    }

    @Override
    public void generateClass(PrintWriter out, String methodName, String ownerMethodName) {
      MethodGenerator mgen = new ExecuteMethodGenerator(origin, needsGuestValueConversion(origin), varArgExpansion, wrapExceptions(origin));;
      out.println("public class " + builtinNode.name() + " extends Node {");
      out.println();

      for (String line : mgen.generate(ownerMethodName, ownerClazz.name(), builtinTypesParamCount)) {
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
              "com.oracle.truffle.api.dsl.Cached",
              "com.oracle.truffle.api.dsl.Specialization",
              "com.oracle.truffle.api.nodes.Node",
              "org.enso.interpreter.dsl.BuiltinMethod",
              "org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode",
              "org.enso.interpreter.runtime.Context",
              "org.enso.interpreter.runtime.builtin.Builtins",
              "org.enso.interpreter.runtime.data.Array",
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

  private abstract class MethodGenerator {
    protected final boolean isStatic;
    protected final boolean isConstructor;
    private final boolean convertToGuestValue;
    protected final TypeWithKind returnTpe;

    public MethodGenerator(boolean isStatic, boolean isConstructor, boolean convertToGuestValue, TypeWithKind returnTpe) {
      this.isStatic = isStatic;
      this.isConstructor = isConstructor;
      this.convertToGuestValue = convertToGuestValue;
      this.returnTpe = returnTpe;
    }

    public abstract List<String> generate(String name, String owner, Map<String, Integer> builtinTypesParameterCounts);

    /**
     * Generate node's `execute` method definition (return type and necessary parameters).
     * '
     * @param owner owner of the method
     * @return string representation of the `execute` method's definition
     */
    protected String methodSigDef(String owner, List<MethodParameter> params, boolean isAbstract) {
      String paramsDef;
      if (params.isEmpty()) {
        paramsDef = "";
      } else {
        paramsDef =
                ", "
                        + StringUtils.join(params.stream().flatMap(x -> x.declaredParameters(expandVararg(x.index))).toArray(), ", ");
      }
      String abstractModifier = isAbstract ? "abstract " : "";
      String thisParamTpe = isStatic || isConstructor || isAbstract ? "Object" : owner;
      return abstractModifier + targetReturnType(returnTpe) + " execute(" + thisParamTpe + " _this" + paramsDef + ")" + (isAbstract ? ";" : "");
    }

    /**
     * Infers the correct return type from the method signature
     * @return String representation of the type to use
     */
    protected String targetReturnType(TypeWithKind tpe) {
      if (isConstructor) return "Object";
      else {
        switch (tpe.kind()) {
          case OBJECT:
            if (tpe.isValidGuestType()) {
              return tpe.baseType();
            } else {
              if (!convertToGuestValue) {
                throw new RuntimeException(
                        "If intended, automatic conversion of value of type " + tpe.baseType
                                + " to guest value requires explicit '@Builtin.ReturningGuestObject' annotation");
              }
              return "Object";
            }
          default:
            return "Object";
        }
      }
    }

    protected abstract Optional<Integer> expandVararg(int paramIndex);
  }

  /**
   * A method generator for an abstract `execute` and at least a single specialization.
   */
  private class SpecializedMethodsGenerator extends MethodGenerator {
    private List<ExecutableElement> elements;

    public SpecializedMethodsGenerator(List<ExecutableElement> elements) {
      this(elements, elements.get(0));
    }
    private SpecializedMethodsGenerator(List<ExecutableElement> elements, ExecutableElement first) {
      this(elements,
              first.getModifiers().contains(Modifier.STATIC),
              first.getKind() == ElementKind.CONSTRUCTOR,
              first.getAnnotation(Builtin.ReturningGuestObject.class) != null,
              TypeWithKind.createFromTpe(first.getReturnType().toString())
              );

      // Make sure all methods were defined the same way, except for paramters' types
      assert(allEqual(elements.stream().map(e -> e.getModifiers().contains(Modifier.STATIC))));
      assert(allEqual(elements.stream().map(e -> e.getKind() == ElementKind.CONSTRUCTOR)));
      assert(allEqual(elements.stream().map(e -> e.getAnnotation(Builtin.ReturningGuestObject.class) != null)));
      assert(allEqual(elements.stream().map(e -> TypeWithKind.createFromTpe(e.getReturnType().toString()))));
    }

    public SpecializedMethodsGenerator(
            List<ExecutableElement> elements,
            boolean isStatic,
            boolean isConstructor,
            boolean convertToGuestValue,
            TypeWithKind returnTpe) {
      super(isStatic, isConstructor, convertToGuestValue, returnTpe);
      this.elements = elements;
    }

    private <T> boolean allEqual(Stream<T> stream) {
      return stream.distinct().count() <= 1;
    }

    @Override
    public List<String> generate(String name, String owner, Map<String, Integer> builtinTypesParameterCounts) {
      SpecializationMeta meta = inferExecuteParameters();
      List<String> result = new ArrayList<>();

      result.add(methodSigDef(owner, meta.execParams(), true));
      result.add("");
      result.addAll(paramsOfSpecializedMethods(meta.diffParam).flatMap(paramsList ->
        specialize(owner, name, paramsList, meta.diffParam).stream()
      ).collect(Collectors.toList()));
      return result;
    }

    @Override
    protected Optional<Integer> expandVararg(int paramIndex) {
      return Optional.empty();
    }

    private Stream<List<MethodParameter>> paramsOfSpecializedMethods(Optional<Integer> specializedParamIdx) {
      Stream<List<MethodParameter>> unsorted = elements.stream().map(method -> {
        List<? extends VariableElement> params = method.getParameters();
        return IntStream.range(0, params.size()).mapToObj(i ->
                fromVariableElementToMethodParameter(i, params.get(i))).collect(Collectors.toList());
      });
      if (specializedParamIdx.isEmpty()) {
        // No need to sort specializations when only dealing with a single one
        return unsorted;
      } else {
        final int specializedParamIdxFinal = specializedParamIdx.get();
        return unsorted.sorted((a, b) ->
          isLessSpecific(a.get(specializedParamIdxFinal)) ? 1 :
                  isLessSpecific(b.get(specializedParamIdxFinal)) ? -1 : 0
        );
      }
    }

    /**
     * Helper method to ensure that specialized methods are not forced to be defined
     * in a specific order.
     *
     * @param p `execute`'s parameter on which specialization is performed
     * @return true, if the parameter's type means that the specialization which contains it should be defined later
     */
    private boolean isLessSpecific(MethodParameter p) {
      return p.tpe().equals("java.lang.Object") || p.tpe().equals("java.lang.String");
    }

    private SpecializationMeta inferExecuteParameters() {
      Map<Integer, List<MethodParameter>> paramss = elements.stream().flatMap(method -> {
        List<? extends VariableElement> params = method.getParameters();
        return IntStream.range(0, params.size()).mapToObj(i ->
                fromVariableElementToMethodParameter(i, params.get(i)));
      }).collect(Collectors.groupingBy(p -> p.index));

      List<Integer> diffParams = new ArrayList<>();
      ArrayList<MethodParameter> execParams = new ArrayList<>();
      ArrayList<MethodParameter> fallbackExecParams = new ArrayList<>();
      paramss.forEach((k, v) -> {
        if (v.size() != elements.size()) {
          throw new RuntimeException("Restriction: Specialized methods have to have equal number of parameters. Expected " + elements.size() + ", got " + v.size());
        }

        boolean allParamsEqual = allEqual(v.stream());
        MethodParameter p = v.get(0);
        if (!needsToInjectValueOfType(p)) {
          if (allParamsEqual && (v.size() > 1)) {
            execParams.add(p);

          } else {
            // Specialize on the given parameter
            execParams.add(new MethodParameter(execParams.size() + 1, p.name(), "Object", new ArrayList<>()));
            diffParams.add(k);
          }
          fallbackExecParams.add(p);
        }
      });

      if (diffParams.isEmpty()) {
        if (elements.size() == 1) {
          return new SpecializationMeta(execParams, Optional.empty());
        }
        throw new RuntimeException("Could not infer the parameter to specialize on based on parameters' types");
      } else if (diffParams.size() > 1) {
        if (elements.size() == 1) {
          // Fallback, cannot infer specialization automatically but with a single method
          // we can confidently just leave params as-is
          return new SpecializationMeta(fallbackExecParams, Optional.empty());
        }
        throw new RuntimeException("Implementation limitation: Builtins DSL infers specialization on a single parameter. Write Node specialization manually instead");
      }
      return new SpecializationMeta(execParams, Optional.of(diffParams.get(0)));
    }

    private boolean needsToInjectValueOfType(MethodParameter p) {
      return typesToInject.contains(p.tpe());
    }

    private final List<String> typesToInject = List.of("org.enso.interpreter.runtime.Context");

    private record SpecializationMeta(List<MethodParameter> execParams, Optional<Integer> diffParam) {}

    /**
     * Generate node's `execute` method definition (return type and necessary parameters).
     * '
     * @param owner owner of the method
     * @return string representation of the `execute` method's definition
     */
    protected List<String> specialize(String owner, String name, List<MethodParameter> params, Optional<Integer> specializedParam) {
      List<SpecializedMethodParameter> params1 = params.stream().map(p -> paramOfSpecializedMethod(p, specializedParam.orElse(-1))).collect(Collectors.toList());
      String paramsDef = "";
      if (!params1.isEmpty()) {
        Object[] allParamDef = params1.stream().flatMap(x -> x.declaredParameter()).toArray();
        if (allParamDef.length > 0) {
          paramsDef = ", " + StringUtils.join(allParamDef, ", ");
        }
      }
      String thisParamTpe = isStatic || isConstructor ? "Object" : owner;
      String suffix = specializedParam.map(idx -> params.get(idx).tpeSimpleName()).orElse("Execute");

      String annotation = "@Specialization";
      String methodSig = targetReturnType(returnTpe) + " do" + suffix + "(" + thisParamTpe + " _this" + paramsDef + ")";
      String paramsApplied;
      if (params1.isEmpty()) {
        paramsApplied = "";
      } else {
        paramsApplied =
                StringUtils.join(params1.stream().map(x -> x.paramName()).toArray(), ", ");
      }

      List<String> specializationDeclaration = new ArrayList<>();
      specializationDeclaration.add(annotation);
      specializationDeclaration.add(methodSig + " {");
      specializationDeclaration.addAll(
              params1
                      .stream()
                      .flatMap(p -> p.auxParamDef().stream())
                      .map(d -> "  " + d)
                      .collect(Collectors.toList()));

      if (isConstructor) {
        specializationDeclaration.add("  return new " + owner + "(" + paramsApplied + ");");
      } else {
        String qual = isStatic ? owner : "_this";
        switch (returnTpe.kind()) {
          case VOID:
            specializationDeclaration.add("  " + qual + "." + name + "(" + paramsApplied + ");");
            specializationDeclaration.add("  return Context.get(this).getBuiltins().nothing().newInstance();");
            break;
          case ARRAY:
            specializationDeclaration.add("  return new Array((Object[]) " + qual + "." + name + "(" + paramsApplied + "));");
            break;
          default:
            specializationDeclaration.add("  return " + qual + "." + name + "(" + paramsApplied + ");");
        }
      }
      specializationDeclaration.add("}");
      return specializationDeclaration;
    }

    abstract class SpecializedMethodParameter {
      abstract String paramName();
      abstract Optional<String> auxParamDef();
      abstract Stream<String> declaredParameter();
    }

    private class RegularMethodParameter extends SpecializedMethodParameter {
      public RegularMethodParameter(MethodParameter param) {
        this.param = param;
      }

      private final MethodParameter param;

      @Override
      String paramName() {
        return param.name;
      }

      @Override
      Optional<String> auxParamDef() {
        return Optional.empty();
      }

      @Override
      Stream<String> declaredParameter() {
        return param.declaredParameters(Optional.empty());
      }
    }


    private class CachedMethodParameter extends SpecializedMethodParameter {
      private final MethodParameter param;
      private final String cachedExpr;
      private final String cacheNode;
      private final String cacheNodeParam;

      public CachedMethodParameter(MethodParameter param, String cachedExpr, String cacheNode, String cacheNodeParam) {
        this.param = param;
        this.cachedExpr = cachedExpr;
        this.cacheNode = cacheNode;
        this.cacheNodeParam = cacheNodeParam;
      }

      @Override
      String paramName() {
        return param.name() + "Cached";
      }

      @Override
      public Optional<String> auxParamDef() {
        return Optional.of(param.tpe() + " " + paramName() + " = " + cacheNodeParam + ".execute(" + param.name() + ");");
      }

      @Override
      public Stream<String> declaredParameter() {
        return Stream.of(
                "Object " + param.name(),
                "@Cached(\"" + cachedExpr + "\") " + cacheNode + " " + cacheNodeParam);
      }
    }

    private class InjectedMethodParameter extends SpecializedMethodParameter {
      private final MethodParameter param;

      public InjectedMethodParameter(MethodParameter param) {
        this.param = param;
      }

      @Override
      String paramName() {
        return param.name();
      }

      @Override
      public Optional<String> auxParamDef() {
        return Optional.of("Context " + param.name() +" = Context.get(this);");
      }

      @Override
      public Stream<String> declaredParameter() {
        return Stream.empty();
      }
    }

    SpecializedMethodParameter paramOfSpecializedMethod(MethodParameter param, int specializedOnParam) {
      if (param.index == specializedOnParam) {
        if (param.tpe.equals("java.lang.String")) {
          return new CachedMethodParameter(param, "build()", "ExpectStringNode", "expectStringNode");
        } else {
          return new RegularMethodParameter(param);
        }
      } else {
        if (needsToInjectValueOfType(param)) {
          return new InjectedMethodParameter(param);
        } else {
          return new RegularMethodParameter(param);
        }
      }
    }

  }


  /**
   * A method generator for a Node with a single `execute` method.
   */
  private class ExecuteMethodGenerator extends MethodGenerator {
    private final List<MethodParameter> params;
    private final int varargExpansion;
    private final boolean needsVarargExpansion;
    private final SafeWrapException[] exceptionWrappers;

    public ExecuteMethodGenerator(ExecutableElement method, boolean convertToGuestValue, int expandedVarargs, SafeWrapException[] exceptionWrappers) {
      this(method, method.getParameters(), convertToGuestValue, expandedVarargs, exceptionWrappers);
    }

    private ExecuteMethodGenerator(
            ExecutableElement method,
            List<? extends VariableElement> params,
            boolean convertToGuestValue,
            int expandedVarargs,
            SafeWrapException[] exceptionWrappers) {
      this(
          method.getReturnType().toString(),
          IntStream.range(0, method.getParameters().size()).mapToObj(i ->
                  fromVariableElementToMethodParameter(i, params.get(i))).collect(Collectors.toList()),
          method.getModifiers().contains(Modifier.STATIC),
          method.getKind() == ElementKind.CONSTRUCTOR,
          convertToGuestValue,
          expandedVarargs,
          method.isVarArgs(),
          exceptionWrappers);
    }

    private ExecuteMethodGenerator(
            String returnTpe,
            List<MethodParameter> params,
            boolean isStatic,
            boolean isConstructor,
            boolean convertToGuestValue,
            int expandedVarargs,
            boolean isVarargs,
            SafeWrapException[] exceptionWrappers) {
      super(isStatic, isConstructor, convertToGuestValue, TypeWithKind.createFromTpe(returnTpe));
      this.params = params;
      this.varargExpansion = expandedVarargs;
      this.needsVarargExpansion = isVarargs && (varargExpansion > 0);
      this.exceptionWrappers = exceptionWrappers;
    }

    @Override
    protected Optional<Integer> expandVararg(int paramIndex) {
      return needsVarargExpansion && params.size() >= (paramIndex + 1) ? Optional.of(varargExpansion) : Optional.empty();
    }


    private String[] auxToHostConversions(MethodParameter param) {
      if (param.needsToHostTranslation()) {
        TypeWithKind tpeWithKind = TypeWithKind.createFromTpe(param.tpe);
        String hostParam = param.hostVarName();
        String tmpObject = "itemsOf" + param.capitalizedName();
        return new String[] {
                "Object[] " + tmpObject + " = " + param.name + ".getItems();",
                tpeWithKind.baseType() + "[] "+ hostParam + " = new " + tpeWithKind.baseType() + "[" + tmpObject + ".length];",
                "for (int i=0; i < " + hostParam + ".length; i++) {",
                "  " + hostParam + "[i] = (" + tpeWithKind.baseType() + ") context.getEnvironment().asHostObject(" + tmpObject + "[i]);",
                "}"
        };
      } else {
        return new String[0];
      }
    }

    private String[] bodyBase(String name, String owner) {
      String paramsApplied;
      if (params.isEmpty()) {
        paramsApplied = "";
      } else {
        paramsApplied =
                StringUtils.join(params.stream().flatMap(x -> x.paramUseNames(expandVararg(x.index))).toArray(), ", ");
      }
      if (isConstructor) {
        return new String[]{"  return new " + owner + "(" + paramsApplied + ");"};
      } else {
        String qual = isStatic ? owner : "_this";
        switch (returnTpe.kind()) {
          case VOID:
            return new String[] {
                    "  " + qual + "." + name + "(" + paramsApplied + ");",
                    "  return Context.get(this).getBuiltins().nothing().newInstance();"
            };
          case ARRAY:
            return new String[]{"  return new Array((Object[]) " + qual + "." + name + "(" + paramsApplied + "));"};
          default:
            if (returnTpe.isValidGuestType()) {
              return new String[]{"  return " + qual + "." + name + "(" + paramsApplied + ");"};
            } else {
              return new String[]{
                      "  return context",
                      "      .getEnvironment()",
                      "      .asGuestValue(" + qual + "." + name + "(" + paramsApplied + "));"};
            }
        }
      }
    }

    private boolean needsContext() {
      boolean result = false;
      // Does the return value need to be translated to a guest value?
      if (!isConstructor && (returnTpe.kind() == TpeKind.OBJECT)) {
        if (!returnTpe.isValidGuestType()) {
          result = true;
        }
      }
      // Do any of params need to be translated to a host object?
      return result || params.stream().anyMatch(p -> p.needsToHostTranslation());
    }

    public List<String> generate(String name, String owner, Map<String, Integer> builtinTypesParameterCounts) {
      boolean wrapsExceptions = exceptionWrappers.length != 0;
      String[] body = bodyBase(name, owner);

      List<String> method = new ArrayList<>();
      method.add(methodSigDef(owner, params, false) + " {");
      if (needsContext()) {
        method.add("  Context context = Context.get(this);");
      }
      if (wrapsExceptions) {;
        method.add("  try {");
        params.stream().forEach(p -> {
          for (String s: auxToHostConversions(p)) {
            method.add("    " + s);
          }
        });
        for (String statement: body) {
          method.add("  " + statement);
        }
        for (int i=0; i < exceptionWrappers.length; i++) {
          method.addAll(exceptionWrappers[i].toCatchClause(params, builtinTypesParameterCounts));
        }
        method.add("  }");
        method.add("}");
      } else {
        params.stream().forEach(p -> {
          for (String s: auxToHostConversions(p)) {
            method.add("    " + s);
          }
        });
        method.addAll(List.of(body));
        method.add("}");
      }
      return method;
    }
  }

  private enum TpeKind {
    VOID,
    ARRAY,
    OBJECT
  }

  /**
   * TypeWithKind provides a convenience wrapper for the types that can be encountered
   * in builtins construction.
   *
   * For example:
   * - Java's `Foo[]` type is of kind `Array` and base type `Foo`
   * - `void` return type is of kind `Void` and base type `Nothing`
   * - all other accepted types are of kind `Object`
   */
  private record TypeWithKind(String baseType, TpeKind kind) {
    static TypeWithKind createFromTpe(String tpeName) {
      if (tpeName.equals("void")) {
        return new TypeWithKind("Nothing", TpeKind.VOID);
      } else if (tpeName.endsWith("[]")) {
        int idx = tpeName.indexOf("[");
        return new TypeWithKind(tpeName.substring(0, idx), TpeKind.ARRAY);
      } else {
        return new TypeWithKind(tpeName, TpeKind.OBJECT);
      }
    }

    // Heuristic
    boolean isValidGuestType() {
      return baseType.startsWith("java.lang") || primitiveTypes.contains(baseType) || validGuestTypes.contains(baseType);
    }

    private final static List<String> primitiveTypes =
            List.of(Boolean.TYPE, Long.TYPE, Double.TYPE, Float.TYPE).stream().map(Class::getSimpleName).collect(Collectors.toList());
    /**
     * A list of hard-coded types that can be used in the parameter or return type position
     * that are valid host types i.e extend TruffleObject.
     * Cannot go via reflection and check that they implement the interface, unfortunately.
     */
    private static List<String> validGuestTypes =
            List.of(
                    "org.enso.interpreter.runtime.data.Array",
                    "org.enso.interpreter.runtime.data.Ref",
                    "org.enso.interpreter.runtime.data.ManagedResource",
                    "org.enso.interpreter.runtime.data.Text",
                    "org.enso.interpreter.runtime.data.EnsoFile");

  }

  private record ClassName(String pkg, String name) {
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
     * Returns a parameter's declaration, consisting of its type and name.
     * If the parameter represents a vararg, the declaration is repeated. Otherwise the method
     * returns a single element Stream of the parameter declaration for the method.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter, potentially repeated for varargs
     */
    public Stream<String> declaredParameters(Optional<Integer> expand) {
      // If the parameter is the expanded vararg we must get rid of the `[]` suffix
      TypeWithKind tpeWithKind = TypeWithKind.createFromTpe(tpe);
      String paramTpe;

      switch (tpeWithKind.kind()) {
        case ARRAY:
          // Expanded varargs are no longer represented as Arrays but as individual elements
          if (expand.isEmpty()) {
            paramTpe = "Array";
          } else {
            paramTpe = "Object";
          }
          break;
        case OBJECT:
          paramTpe = tpeWithKind.baseType();
          break;
        default:
          throw new RuntimeException("Invalid type for parameter " + name);
      }
      String paramAnnotations = annotations.isEmpty() ? "" : (StringUtils.joinWith(" ", annotations.toArray()) + " ");
      return names(expand).map(n -> paramAnnotations + paramTpe + " " + n);
    }

    /**
     * Parameter's name, capitalized.
     *
     * @return Capitalized parameter's name.
     */
    public String capitalizedName() {
      return this.name.substring(0,1).toUpperCase() + this.name.substring(1);
    }

    /**
     * Determines if the parameter guest object needs to be translated to host representation.
     *
     * @return true, if it needs, false otherwise.
     */
    public boolean needsToHostTranslation() {
      TypeWithKind tpeWithKind = TypeWithKind.createFromTpe(tpe);
      switch (tpeWithKind.kind()) {
        case ARRAY:
          return !tpeWithKind.isValidGuestType();
        default:
          return false;
      }
    }

    /**
     * Name of the variable to store host representation of polyglot object coming from the parameter.
     * See {@link MethodParameter#needsToHostTranslation()}.
     *
     * @return name of the variable that stores host representation of the polyglot object.
     */
    public String hostVarName() {
      return "host" + capitalizedName();
    }

    /**
     * Returns a parameter's name.
     * If the parameter represents a vararg, the name is repeated. Otherwise return a single
     * element Stream of declarations.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter variable, potentially repeated for varargs
     */
    public Stream<String> names(Optional<Integer> expand) {
      return expand.map(e->
        IntStream.range(0, e).mapToObj(i-> name + "_" + (i+1))
      ).orElse(Stream.of(name));
    }

    /**
     * Returns a parameter's name when used in the body of the method.
     * Compared to {@link MethodParameter#names} it will take into account potential
     * translation to Java host representation.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter variable, potetnially repeated for varargs
     */
    public Stream<String> paramUseNames(Optional<Integer> expand) {
      if (needsToHostTranslation()) {
        return Stream.of(hostVarName());
      } else {
        return expand.map(e->
                IntStream.range(0, e).mapToObj(i-> name + "_" + (i+1))
        ).orElse(Stream.of(name));
      }
    }

    public String tpeSimpleName() {
      String[] tpeElements = tpe.split("\\.");
      return tpeElements[tpeElements.length - 1];
    }

  }

  /**
   * Wrapper around {@link Builtin.WrapException} annotation with all elements of Class type resolved.
   * extracted
   */
  private record SafeWrapException(Attribute.Class from, Attribute.Class to, Boolean passException) {

    public SafeWrapException(Attribute.Class from, Attribute.Class to, Attribute.Constant propagate) {
      this(from, to, propagate != null ? (Boolean) propagate.getValue() : false);
    }

    /**
     * Generate a catch-clause that catches `from`, wraps it into `to` Enso type and rethrows the latter
     * @param methodParameters list of all method's parameters, potentially to be applied to `to` constructor
     * @param builtinTypesParameterCounts a map from builtin errors to the number of parameters in their constructors
     * @return Lines representing the (unclosed) catch-clause catching the runtime `from` exception
     */
    List<String> toCatchClause(List<MethodParameter> methodParameters, Map<String, Integer> builtinTypesParameterCounts) {
      String from = fromAttributeToClassName(from(), true);
      String to = fromAttributeToClassName(to(), false);
      if (passException) {
        return List.of(
                "  } catch (" + from + " e) {",
                "    Builtins builtins = Context.get(this).getBuiltins();",
                "    throw new PanicException(builtins.error().make" + to + "(e), this);"
        );
      } else {
        int toParamCount = errorParametersCount(to(), builtinTypesParameterCounts);
        List<String> errorParameters =
                methodParameters
                        .stream()
                        .limit(toParamCount - 1)
                        .flatMap(x -> x.names(Optional.empty()))
                        .collect(Collectors.toList());
        String errorParameterCode = errorParameters.isEmpty() ? "" : ", " + StringUtils.join(errorParameters, ", ");
        return List.of(
                "  } catch (" + from + " e) {",
                "    Builtins builtins = Context.get(this).getBuiltins();",
                "    throw new PanicException(builtins.error().make" + to + "(_this" + errorParameterCode + "), this);"
        );
      }
    }

    private String fromAttributeToClassName(Attribute.Class clazz, Boolean fullName) {
      String baseType = clazz.classType.baseType().toString();
      if (fullName) return baseType;
      else {
        String[] clazzElements = baseType.split("\\.");
        if (clazzElements.length == 0) {
          return baseType;
        } else {
          return clazzElements[clazzElements.length - 1];
        }
      }
    }

    private int errorParametersCount(Attribute.Class clazz, Map<String, Integer> builtinTypesParameterCounts) {
      String clazzSimple = fromAttributeToClassName(clazz, false);
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
  private static class WrapExceptionExtractor {

    private static final String FromElementName = "from";
    private static final String ToElementName = "to";
    private static final String PropagateElementName = "propagate";
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
    public SafeWrapException[] extract(ProcessingEnvironment processingEnv, Element element) {
      if (element.getAnnotation(wrapExceptionsAnnotationClass) != null) {
        return extractClassElementFromAnnotationContainer(processingEnv, element, wrapExceptionsAnnotationClass);
      } else if (element.getAnnotation(wrapExceptionAnnotationClass) != null) {
        return extractClassElementFromAnnotation(processingEnv, element, wrapExceptionAnnotationClass);
      } else {
        return new SafeWrapException[0];
      }
    }

    private SafeWrapException[] extractClassElementFromAnnotation(ProcessingEnvironment processingEnv, Element element, Class<?> annotationClass) {
      Element builtinElement = processingEnv.getElementUtils().getTypeElement(annotationClass.getCanonicalName());
      TypeMirror builtinType = builtinElement.asType();

      List<SafeWrapException> exceptionWrappers = new ArrayList<>();
      for (AnnotationMirror am: element.getAnnotationMirrors()) {
        if (am.getAnnotationType().equals(builtinType)) {
          Attribute.Class valueFrom = null;
          Attribute.Class valueTo = null;
          Attribute.Constant valuePropagate = null;
          for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry : am.getElementValues().entrySet() ) {
            Name key = entry.getKey().getSimpleName();
            if (key.toString().equals(FromElementName)) {
              valueFrom = (Attribute.Class)(entry.getValue());
            } else if (key.toString().equals(ToElementName)) {
              valueTo = (Attribute.Class)(entry.getValue());
            } else if (key.toString().equals(PropagateElementName)) {
              valuePropagate = (Attribute.Constant)(entry.getValue());
            }
          }
          if (valueFrom != null && valueTo != null) {
            exceptionWrappers.add(new SafeWrapException(valueFrom, valueTo, valuePropagate));
          }
        }
      }
      return exceptionWrappers.toArray(new SafeWrapException[0]);
    }


    private SafeWrapException[] extractClassElementFromAnnotationContainer(ProcessingEnvironment processingEnv, Element element, Class<?> annotationClass) {

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
                Attribute.Constant valuePropagate = null;
                Attribute.Compound attr = (Attribute.Compound)wrapExceptions.values[i];
                for (Pair<Symbol.MethodSymbol, Attribute> p: attr.values) {
                  Name key = p.fst.getSimpleName();
                  if (key.contentEquals(FromElementName)) {
                    valueFrom = (Attribute.Class)p.snd;
                  } else if (key.contentEquals(ToElementName)) {
                    valueTo = (Attribute.Class)p.snd;
                  } else if (key.contentEquals(PropagateElementName)) {
                    valuePropagate = (Attribute.Constant)p.snd;
                  }
                }
                if (valueFrom != null && valueTo != null) {
                  SafeWrapException converted = new SafeWrapException(valueFrom, valueTo, valuePropagate);
                  wrappedExceptions.add(converted);
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
