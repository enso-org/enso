package org.enso.interpreter.dsl;

import com.google.auto.service.AutoService;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Set;

@SupportedAnnotationTypes("org.enso.interpreter.dsl.BuiltinMethod")
@SupportedSourceVersion(SourceVersion.RELEASE_8)
@AutoService(Processor.class)
public class MethodProcessor extends AbstractProcessor {

  void print(Object o) {
    processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING, o.toString());
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    processingEnv
        .getMessager()
        .printMessage(Diagnostic.Kind.WARNING, "I'm running and I ain't stoppin'");

    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      print(annotatedElements);
      for (Element elt : annotatedElements) {
        TypeElement element = (TypeElement) elt;
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
                          .printMessage(Diagnostic.Kind.ERROR, "No execute method.");
                      return null;
                    });
        if (executeMethod == null) return true;
        print(executeMethod);
        String pkgName =
            processingEnv.getElementUtils().getPackageOf(element).getQualifiedName().toString();
        String className = element.getSimpleName().toString();
        print(pkgName);
        print(className);
        try {
          generateCode(pkgName, className, executeMethod);
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }

    return true;
  }

  private void generateCode(String pkg, String className, ExecutableElement executeMethod)
      throws IOException {
    JavaFileObject gen =
        processingEnv.getFiler().createSourceFile(pkg + "." + className + "MethodGen");
    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("public class " + className + "MethodGen {}");
    }
  }
}
