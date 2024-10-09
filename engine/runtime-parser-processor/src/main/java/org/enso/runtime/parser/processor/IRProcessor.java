package org.enso.runtime.parser.processor;

import java.util.Set;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic.Kind;
import org.enso.runtime.parser.dsl.IRNode;

@SupportedAnnotationTypes({
  "org.enso.runtime.parser.dsl.IRNode",
  "org.enso.runtime.parser.dsl.IRChild"
})
public class IRProcessor extends AbstractProcessor {

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    var irNodeElems = roundEnv.getElementsAnnotatedWith(IRNode.class);
    for (var irNodeElem : irNodeElems) {
      processIrNode(irNodeElem);
    }
    return true;
  }

  private void processIrNode(Element irNodeElem) {
    if (irNodeElem.getKind() != ElementKind.INTERFACE) {
      printError("IRNode annotation can only be applied to interfaces", irNodeElem);
    }
    if (!isSubtypeOfIR(irNodeElem.asType())) {
      printError("Interface annotated with @IRNode must be a subtype of IR interface", irNodeElem);
    }
  }

  private boolean isSubtypeOfIR(TypeMirror type) {
    var irType =
        processingEnv.getElementUtils().getTypeElement("org.enso.compiler.core.IR").asType();
    return processingEnv.getTypeUtils().isAssignable(type, irType);
  }

  private void printError(String msg, Element elem) {
    processingEnv.getMessager().printMessage(Kind.ERROR, msg, elem);
  }
}
