package org.enso.interpreter.dsl;

import org.enso.interpreter.dsl.builtins.*;

import com.google.common.base.CaseFormat;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.openide.util.lookup.ServiceProvider;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;

import java.util.stream.IntStream;

/**
 * The processor used to generate code from the methods of the runtime representations annotated
 * with {@link Builtin}.
 *
 * <p>It represents the first stage in generating the final builtin nodes. The processor will
 * generate a corresponding @BuiltinMethod class which, in turn, will get processed by another
 * processor and deliver a RootNode for the method.
 */
@SupportedAnnotationTypes({
  "org.enso.interpreter.dsl.Builtin",
  "org.enso.interpreter.dsl.Builtin.Method"
})
@ServiceProvider(service = Processor.class)
public class BuiltinsProcessor extends AbstractProcessor {

  private static final String BuiltinsPkg = "org.enso.interpreter.node.expression.builtin";

  private record Specialized(String owner, String methodName) {}

  private final Map<Specialized, List<ExecutableElement>> specializedMethods = new HashMap<>();

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
    String clazzName =
        annotation.name().isEmpty() ? element.getSimpleName().toString() : annotation.name();
    String builtinPkg =
        annotation.pkg().isEmpty() ? BuiltinsPkg : BuiltinsPkg + "." + annotation.pkg();
    ClassName builtinType = new ClassName(builtinPkg, clazzName);
    JavaFileObject gen =
        processingEnv.getFiler().createSourceFile(builtinType.fullyQualifiedName());
    Optional<String> stdLibName =
        annotation.stdlibName().isEmpty() ? Optional.empty() : Optional.of(annotation.stdlibName());
    generateBuiltinType(gen, builtinType, stdLibName, elt.getSimpleName().toString());
  }

  private void generateBuiltinType(
      JavaFileObject gen, ClassName builtinType, Optional<String> stdLibName, String underlyingTypeName) throws IOException {
    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + builtinType.pkg() + ";");
      out.println();
      for (String importPkg : typeNecessaryImports) {
        out.println("import " + importPkg + ";");
      }
      out.println();
      String builtinTypeAnnotation = "@BuiltinType(" + stdLibName.map(n -> "name = \"" + n + "\", ").orElse("") + "underlyingTypeName = \"" + underlyingTypeName + "\")";
      out.println(builtinTypeAnnotation);
      out.println("public class " + builtinType.name() + " extends Builtin {");
      out.println("""
        @Override
        protected boolean containsValues() {
          return true;
        }
      }
      """);
    }
  }

  /**
   * Generate a @BuiltinMethod node class
   *
   * @param element method annotated with @Builtin.Method which implementation and signature will be
   *     used to generate the target node class
   * @param roundEnv meta information about the current round of processing
   */
  public void handleMethodElement(Element element, RoundEnvironment roundEnv) throws IOException {
    ExecutableElement method = (ExecutableElement) element;
    Element owner = element.getEnclosingElement();

    if (owner.getKind() == ElementKind.CLASS) {
      Builtin ownerAnnotation = owner.getAnnotation(Builtin.class);
      TypeElement ownerTpeElement = (TypeElement) owner;
      String ownerName;
      String builtinPkg;
      if (ownerAnnotation != null) {
        ownerName =
            ownerAnnotation.name().isEmpty()
                ? ownerTpeElement.getSimpleName().toString()
                : ownerAnnotation.name();

        builtinPkg =
            ownerAnnotation.pkg().isEmpty()
                ? BuiltinsPkg
                : BuiltinsPkg + "." + ownerAnnotation.pkg();
      } else {
        ownerName = ownerTpeElement.getSimpleName().toString();
        builtinPkg = BuiltinsPkg;
      }

      PackageElement pkgElement = (PackageElement) ownerTpeElement.getEnclosingElement();
      Builtin.Method annotation = element.getAnnotation(Builtin.Method.class);
      boolean isConstructor = method.getKind() == ElementKind.CONSTRUCTOR;

      if (annotation.expandVarargs() != 0) {
        if (annotation.expandVarargs() < 0)
          throw new RuntimeException(
              "Invalid varargs value in @Builtin annotation. Must be positive");
        if (!annotation.name().isEmpty())
          throw new RuntimeException("Name cannot be non-empty when varargs are used");

        IntStream.rangeClosed(1, annotation.expandVarargs())
            .forEach(
                i -> {
                  String methodName =
                      (isConstructor ? "new" : method.getSimpleName().toString()) + "_" + i;
                  String clazzName =
                      CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, methodName);
                  ClassName builtinMethodNode =
                      new ClassName(builtinPkg, clazzName + ownerName + "Node");
                  ClassName ownerClass =
                      new ClassName(
                          pkgElement.getQualifiedName().toString(),
                          ownerTpeElement.getSimpleName().toString());
                  ClassName stdLibOwnerClass =
                      new ClassName(pkgElement.getQualifiedName().toString(), ownerName);

                  try {
                    MethodNodeClassGenerator classGenerator =
                        new NoSpecializationClassGenerator(
                            method, builtinMethodNode, ownerClass, stdLibOwnerClass, i);
                    classGenerator.generate(
                        processingEnv,
                        methodName,
                        annotation.description(),
                        method.getSimpleName().toString(),
                        annotation.autoRegister());
                  } catch (IOException ioe) {
                    throw new RuntimeException(ioe);
                  }
                });
      } else {
        String builtinMethodName =
            !annotation.name().isEmpty()
                ? annotation.name()
                : isConstructor ? "new" : method.getSimpleName().toString();
        String builtinMethodNameClass =
            !annotation.name().isEmpty()
                ? CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, annotation.name())
                : isConstructor ? "new" : method.getSimpleName().toString();
        Builtin.Specialize specialize = element.getAnnotation(Builtin.Specialize.class);

        ClassName builtinMethodNode =
            new ClassName(
                builtinPkg,
                CaseFormat.LOWER_CAMEL.to(CaseFormat.UPPER_CAMEL, builtinMethodNameClass)
                    + ownerName
                    + "Node");
        ClassName ownerClass =
            new ClassName(
                pkgElement.getQualifiedName().toString(),
                ownerTpeElement.getSimpleName().toString());
        ClassName stdLibOwnerClass =
            new ClassName(pkgElement.getQualifiedName().toString(), ownerName);

        int expected = specializationsCount(owner, builtinMethodName);
        if (specialize != null) {
          Specialized key = new Specialized(ownerName, builtinMethodName);
          List<ExecutableElement> encountered =
              specializedMethods.compute(
                  key,
                  (k, v) -> {
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
                new SpecializationClassGenerator(
                    encountered, builtinMethodNode, ownerClass, stdLibOwnerClass);
            classGenerator.generate(
                processingEnv,
                builtinMethodName,
                annotation.description(),
                method.getSimpleName().toString(),
                annotation.autoRegister());
          }
        } else {
          MethodNodeClassGenerator classGenerator =
              new NoSpecializationClassGenerator(
                  method, builtinMethodNode, ownerClass, stdLibOwnerClass);
          classGenerator.generate(
              processingEnv,
              builtinMethodName,
              annotation.description(),
              method.getSimpleName().toString(),
              annotation.autoRegister());
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
    return (int)
        owner.getEnclosedElements().stream()
            .filter(
                e -> {
                  if (e.getKind() != ElementKind.METHOD && e.getKind() != ElementKind.CONSTRUCTOR)
                    return false;
                  Builtin.Method annotation = e.getAnnotation(Builtin.Method.class);
                  Builtin.Specialize specializedAnnot = e.getAnnotation(Builtin.Specialize.class);
                  if (annotation == null || specializedAnnot == null) return false;
                  boolean isConstructor = e.getKind() == ElementKind.CONSTRUCTOR;
                  String name =
                      !annotation.name().isEmpty()
                          ? annotation.name()
                          : isConstructor ? "new" : e.getSimpleName().toString();
                  return name.equals(builtinMethodName);
                })
            .count();
  }

  private final List<String> typeNecessaryImports =
      Arrays.asList(
          "org.enso.interpreter.dsl.BuiltinType",
          "org.enso.interpreter.node.expression.builtin.Builtin");

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }
}
