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
import java.util.Set;
import java.util.stream.Collectors;

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

  public void handleMethodElement(Element element, RoundEnvironment roundEnv) throws IOException {
    ExecutableElement method = (ExecutableElement) element;
    Element owner = element.getEnclosingElement();
    if (owner.getKind() == ElementKind.CLASS) {
      TypeElement tpeElement = (TypeElement) owner;
      PackageElement pkgElement = (PackageElement) tpeElement.getEnclosingElement();
      Builtin annotation = element.getAnnotation(Builtin.class);
      boolean isConstructor = method.getKind() == ElementKind.CONSTRUCTOR;
      String methodName =
              !annotation.name().isEmpty() ? annotation.name() :
                      isConstructor ? "new" : method.getSimpleName().toString();
      String builtinPkg =
          annotation.pkg().isEmpty() ? BuiltinsPkg : BuiltinsPkg + "." + annotation.pkg();
      String optConstrSuffix = isConstructor ? tpeElement.getSimpleName().toString() : "";
      ClassName builtinMethodNode =
          new ClassName(
              builtinPkg,
              methodName.substring(0, 1).toUpperCase() + methodName.substring(1) + optConstrSuffix + "Node");
      ClassName ownerClass =
          new ClassName(pkgElement.getQualifiedName(), tpeElement.getSimpleName());
      JavaFileObject gen =
          processingEnv.getFiler().createSourceFile(builtinMethodNode.fullyQualifiedName());
      MethodGenerator methodGen = new MethodGenerator(method);
      generateBuiltinMethodNode(
          gen, methodGen, methodName, method.getSimpleName().toString(), annotation.description(), builtinMethodNode, ownerClass);
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
      for (String importPkg : necessaryImports) {
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

  private final List<String> necessaryImports =
      Arrays.asList("com.oracle.truffle.api.nodes.Node", "org.enso.interpreter.dsl.BuiltinMethod");

  private MethodParameter fromVariableElementToMethodParameter(VariableElement v) {
    return new MethodParameter(v.getSimpleName().toString(), v.asType().toString());
  }

  /** Method generator encapsulates the generation of the `execute` method. */
  private class MethodGenerator {
    private final String returnTpe;
    private final List<MethodParameter> params;
    private final boolean isStatic;
    private final boolean isConstructor;

    public MethodGenerator(ExecutableElement method) {
      this(
          method.getReturnType().toString(),
          method.getParameters().stream()
              .map(v -> fromVariableElementToMethodParameter(v))
              .collect(Collectors.toList()),
          method.getModifiers().contains(Modifier.STATIC),
          method.getKind() == ElementKind.CONSTRUCTOR);
    }

    private MethodGenerator(String returnTpe, List<MethodParameter> params, boolean isStatic, boolean isConstructor) {
      this.returnTpe = returnTpe;
      this.params = params;
      this.isStatic = isStatic;
      this.isConstructor = isConstructor;
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
                + StringUtils.join(params.stream().map(x -> x.declaredParameter()).toArray(), ", ");
        paramsApplied =
            StringUtils.join(params.stream().map(x -> x.name()).toArray(), ", ");
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

  private record MethodParameter(String name, String tpe) {
    public String declaredParameter() {
      return tpe + " " + name;
    }
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }
}
