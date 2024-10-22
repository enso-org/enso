package org.enso.runtime.parser.processor;

import java.util.ArrayDeque;
import java.util.stream.Collectors;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic.Kind;

final class Utils {
  private Utils() {}

  /** Returns true if the given {@code type} is a subtype of {@code org.enso.compiler.core.IR}. */
  static boolean isSubtypeOfIR(TypeElement type, ProcessingEnvironment processingEnv) {
    var interfacesToProcess = new ArrayDeque<TypeElement>();
    interfacesToProcess.add(type);
    while (!interfacesToProcess.isEmpty()) {
      var current = interfacesToProcess.pop();
      if (current.getSimpleName().toString().equals("IR")) {
        // current.getQualifiedName().toString() returns only "IR" as well, so we can't use it.
        // This is because runtime-parser-processor project does not depend on runtime-parser and
        // so the org.enso.compiler.core.IR interface is not available in the classpath.
        return true;
      }
      // Add all super interfaces to the queue
      for (var superInterface : current.getInterfaces()) {
        var superInterfaceElem = processingEnv.getTypeUtils().asElement(superInterface);
        if (superInterfaceElem instanceof TypeElement superInterfaceTypeElem) {
          interfacesToProcess.add(superInterfaceTypeElem);
        }
      }
    }
    return false;
  }

  /** Returns true if the given type extends {@link org.enso.compiler.core.ir.Expression} */
  static boolean isSubtypeOfExpression(TypeMirror type, ProcessingEnvironment processingEnv) {
    var expressionType =
        processingEnv
            .getElementUtils()
            .getTypeElement("org.enso.compiler.core.ir.Expression")
            .asType();
    return processingEnv.getTypeUtils().isAssignable(type, expressionType);
  }

  static void printError(String msg, Element elem, Messager messager) {
    messager.printMessage(Kind.ERROR, msg, elem);
  }

  static String indent(String code, int indentation) {
    return code.lines()
        .map(line -> " ".repeat(indentation) + line)
        .collect(Collectors.joining(System.lineSeparator()));
  }
}
