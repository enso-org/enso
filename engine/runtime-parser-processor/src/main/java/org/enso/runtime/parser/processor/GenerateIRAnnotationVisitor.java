package org.enso.runtime.parser.processor;

import java.util.LinkedHashSet;
import java.util.List;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleAnnotationValueVisitor14;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.processor.utils.Utils;

final class GenerateIRAnnotationVisitor extends SimpleAnnotationValueVisitor14<Void, Void> {
  private final ProcessingEnvironment procEnv;
  private final ExecutableElement annotationField;
  private final LinkedHashSet<TypeElement> allInterfaces = new LinkedHashSet<>();
  private TypeElement irInterface;

  GenerateIRAnnotationVisitor(ProcessingEnvironment procEnv, ExecutableElement annotationField) {
    this.procEnv = procEnv;
    this.annotationField = annotationField;
    allInterfaces.add(Utils.irTypeElement(procEnv));
  }

  @Override
  public Void visitArray(List<? extends AnnotationValue> vals, Void unused) {
    for (var val : vals) {
      val.accept(this, null);
    }
    return null;
  }

  @Override
  public Void visitType(TypeMirror t, Void unused) {
    var typeElem = (TypeElement) procEnv.getTypeUtils().asElement(t);
    if (Utils.isSubtypeOfIR(typeElem, procEnv)) {
      if (irInterface != null) {
        throw new IRProcessingException(
            "Only one interface can be specified as the IR interface, but found multiple: "
                + irInterface
                + " and "
                + typeElem,
            annotationField);
      }
      irInterface = typeElem;
    }
    allInterfaces.add(typeElem);
    return null;
  }

  /**
   * Returns list of all the interfaces specified in {@link GenerateIR#interfaces()}. May be empty.
   */
  public List<TypeElement> getAllInterfaces() {
    return allInterfaces.stream().toList();
  }

  /**
   * Returns a type from {@link GenerateIR#interfaces()} that is a subtype of {@code
   * org.enso.compiler.core.IR}. There must be only one such subtype specified.
   *
   * @return If there is no interface that is a subtype of {@code org.enso.compiler.core.IR} in the
   *     {@link GenerateIR#interfaces()}, returns {@code null}. Otherwise, returns the interface.
   *     Note that if null is returned, {@code org.enso.compiler.core.IR} should be used. See {@link
   *     GenerateIR#interfaces()}.
   */
  public TypeElement getIrInterface() {
    return irInterface;
  }
}
