package org.enso.runtime.parser.processor;

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

  static void printError(String msg, Element elem, Messager messager) {
    messager.printMessage(Kind.ERROR, msg, elem);
  }
}
