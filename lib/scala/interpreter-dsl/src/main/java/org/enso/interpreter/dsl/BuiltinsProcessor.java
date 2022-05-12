package org.enso.interpreter.dsl;

import org.apache.commons.lang3.StringUtils;
import org.openide.util.lookup.ServiceProvider;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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
@SupportedAnnotationTypes("org.enso.interpreter.dsl.Builtin")
@ServiceProvider(service = Processor.class)
public class BuiltinsProcessor extends AbstractProcessor {

  private static final String BuiltinsPkg = "org.enso.interpreter.node.expression.builtin";

  @Override
  public final boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      for (Element elt : annotatedElements) {
        if (elt.getKind() == ElementKind.METHOD || elt.getKind() == ElementKind.CONSTRUCTOR) {
          try {
            handleMethodElement(elt, roundEnv);
          } catch (Exception ioe) {
            processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, ioe.getMessage());
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
    Optional<String> stdLibName = annotation.name().isEmpty() ? Optional.empty() : Optional.of(annotation.name());
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
      TypeElement tpeElement = (TypeElement) owner;
      PackageElement pkgElement = (PackageElement) tpeElement.getEnclosingElement();
      Builtin annotation = element.getAnnotation(Builtin.class);
      boolean isConstructor = method.getKind() == ElementKind.CONSTRUCTOR;
      String builtinPkg =
          annotation.pkg().isEmpty() ? BuiltinsPkg : BuiltinsPkg + "." + annotation.pkg();

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
            MethodGenerator methodGen = new MethodGenerator(method, i);
            generateBuiltinMethodNode(
                    gen, methodGen, methodName, method.getSimpleName().toString(), annotation.description(), builtinMethodNode, ownerClass);
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
        MethodGenerator methodGen = new MethodGenerator(method, 0);
        generateBuiltinMethodNode(
                gen, methodGen, methodName, method.getSimpleName().toString(), annotation.description(), builtinMethodNode, ownerClass);
      }
    } else {
      throw new RuntimeException("@Builtin method must be owned by the class");
    }
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
   * @throws IOException throws an exception when we cannot write the new class
   */
  private void generateBuiltinMethodNode(
      JavaFileObject gen,
      MethodGenerator mgen,
      String methodName,
      String ownerMethodName,
      String description,
      ClassName builtinNode,
      ClassName ownerClazz)
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

      for (String line : mgen.generateMethod(ownerMethodName, ownerClazz.name())) {
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
      Arrays.asList("com.oracle.truffle.api.nodes.Node", "org.enso.interpreter.dsl.BuiltinMethod");

  private MethodParameter fromVariableElementToMethodParameter(int i, VariableElement v) {
    return new MethodParameter(i, v.getSimpleName().toString(), v.asType().toString());
  }

  /** Method generator encapsulates the generation of the `execute` method. */
  private class MethodGenerator {
    private final String returnTpe;
    private final List<MethodParameter> params;
    private final boolean isStatic;
    private final boolean isConstructor;
    private final int varargExpansion;
    private final boolean needsVarargExpansion;

    public MethodGenerator(ExecutableElement method, int expandedVarargs) {
      this(method, method.getParameters(), expandedVarargs);
    }

    private MethodGenerator(ExecutableElement method, List<? extends VariableElement> params, int expandedVarargs) {
      this(
          method.getReturnType().toString(),
          IntStream.range(0, method.getParameters().size()).mapToObj(i ->
                  fromVariableElementToMethodParameter(i, params.get(i))).collect(Collectors.toList()),
          method.getModifiers().contains(Modifier.STATIC),
          method.getKind() == ElementKind.CONSTRUCTOR,
          expandedVarargs,
          method.isVarArgs());
    }

    private MethodGenerator(
            String returnTpe,
            List<MethodParameter> params,
            boolean isStatic,
            boolean isConstructor,
            int expandedVarargs,
            boolean isVarargs) {
      this.returnTpe = returnTpe;
      this.params = params;
      this.isStatic = isStatic;
      this.isConstructor = isConstructor;
      this.varargExpansion = expandedVarargs;
      this.needsVarargExpansion = isVarargs && (varargExpansion > 0);
    }

    private Optional<Integer> expandVararg(int paramIndex) {
      return needsVarargExpansion && params.size() >= (paramIndex + 1) ? Optional.of(varargExpansion) : Optional.empty();
    }

    public String[] generateMethod(String name, String owner) {
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
      return new String[] {
        targetReturnTpe + " execute(" + thisParamTpe + " _this" + paramsDef + ") {",
        body,
        "}"
      };
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
  private record MethodParameter(int index, String name, String tpe) {
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
      return names(expand).map(n -> parameterTpe + " " + n);
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

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }
}
