package org.enso.runtime.parser.processor;

import java.util.stream.Collectors;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic.Kind;

final class Utils {
  private Utils() {}

  static boolean isSubtypeOfIR(TypeMirror type, ProcessingEnvironment processingEnv) {
    var irType =
        processingEnv.getElementUtils().getTypeElement("org.enso.compiler.core.IR").asType();
    return processingEnv.getTypeUtils().isAssignable(type, irType);
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
