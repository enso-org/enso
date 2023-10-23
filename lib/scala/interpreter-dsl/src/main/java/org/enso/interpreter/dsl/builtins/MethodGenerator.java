package org.enso.interpreter.dsl.builtins;

import com.google.common.base.CaseFormat;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleAnnotationValueVisitor14;
import javax.lang.model.util.SimpleElementVisitor14;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic.Kind;
import org.apache.commons.lang3.StringUtils;
import org.enso.interpreter.dsl.Builtin;

public abstract class MethodGenerator {
  protected final boolean isStatic;
  protected final boolean isConstructor;
  protected final boolean convertToGuestValue;
  protected final TypeWithKind returnTpe;
  protected final ProcessingEnvironment processingEnvironment;
  private static final String FROM_ELEMENT_NAME = "from";
  private static final String TO_ELEMENT_NAME = "to";
  private static final Class<? extends Annotation> wrapExceptionAnnotationClass = Builtin.WrapException.class;
  private static final Class<? extends Annotation> wrapExceptionsAnnotationClass = Builtin.WrapExceptions.class;

  protected SafeWrapException[] wrapExceptions(
      Element element) {
    return extractExceptions(element);
  }

  public MethodGenerator(
      ProcessingEnvironment processingEnvironment,
      boolean isStatic,
      boolean isConstructor,
      boolean convertToGuestValue,
      TypeWithKind returnTpe) {
    this.processingEnvironment = processingEnvironment;
    this.isStatic = isStatic;
    this.isConstructor = isConstructor;
    this.convertToGuestValue = convertToGuestValue;
    this.returnTpe = returnTpe;
  }

  public abstract List<String> generate(String name, String owner);

  /**
   * Generate node's `execute` method definition (return type and necessary parameters). '
   *
   * @param owner owner of the method
   * @return string representation of the `execute` method's definition
   */
  protected String methodSigDef(String owner, List<MethodParameter> params, boolean isAbstract) {
    int paramsLen = params.size();
    String paramsDef;
    boolean includeSelfParam = !(isStatic || isConstructor);
    if (params.isEmpty()) {
      paramsDef = "";
    } else {
      paramsDef =
          (includeSelfParam ? ", " : "")
              + StringUtils.join(
                  params.stream()
                      .flatMap(x -> x.declaredParameters(expandVararg(paramsLen, x.index())))
                      .toArray(),
                  ", ");
    }
    String abstractModifier = isAbstract ? "abstract " : "";
    String thisParamTpe = isStatic || isConstructor ? (isAbstract ? "Object" : owner) : owner;
    return abstractModifier
        + targetReturnType(returnTpe)
        + " execute("
        + (includeSelfParam ? (thisParamTpe + " self") : "")
        + paramsDef
        + ")"
        + (isAbstract ? ";" : "");
  }

  /**
   * Infers the correct return type from the method signature
   *
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
              processingEnvironment
                  .getMessager()
                  .printMessage(
                      Kind.ERROR,
                      "Automatic conversion of value of type "
                          + tpe.baseType()
                          + " to guest value requires explicit '@Builtin.ReturningGuestObject' annotation");
            }
            return "Object";
          }
        default:
          return "Object";
      }
    }
  }

  /**
   * Convert annotated method's variable to MethodParameter
   *
   * @param i position of the variable representing the parameter
   * @param v variable element representing the parameter
   * @return MethodParameter encapsulating the method's parameter info
   */
  protected MethodParameter fromVariableElementToMethodParameter(
      ProcessingEnvironment processingEnv, int i, VariableElement v) {
    String ensoName =
        CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, v.getSimpleName().toString());
    TypeWithKind tpe = TypeWithKind.createFromTpe(v.asType().toString());
    if (tpe.kind() == TypeKind.ARRAY && !tpe.isValidGuestType()) {
      processingEnv
          .getMessager()
          .printMessage(
              Kind.ERROR,
              "Parameter "
                  + v
                  + " is an array of host objects, which "
                  + "is not supported by the MethodGenerator. Either use array of primitive, or valid guest objects, "
                  + "or accept the array as Object and transform it in the method body.",
              v);
    }
    return new MethodParameter(
        i,
        ensoName,
        v.asType().toString(),
        v.getAnnotationMirrors().stream().map(Object::toString).collect(Collectors.toList()));
  }

  protected abstract Optional<Integer> expandVararg(int paramsLen, int paramIndex);

  /**
   * Extract {@link org.enso.interpreter.dsl.Builtin.WrapException} from the annotated element in
   * a mirror-safe manner.
   *
   * @param element a method annotated with either {@link
   *     org.enso.interpreter.dsl.Builtin.WrapException} or {@link
   *     org.enso.interpreter.dsl.Builtin.WrapExceptions}
   * @return An array of safely retrieved (potentially repeated) values of {@link
   *     org.enso.interpreter.dsl.Builtin.WrapException} annotation(s)
   */
  private SafeWrapException[] extractExceptions(Element element) {
    if (element.getAnnotation(wrapExceptionsAnnotationClass) != null) {
      return extractClassElementFromAnnotationContainer(element);
    } else if (element.getAnnotation(wrapExceptionAnnotationClass) != null) {
      return extractClassElementFromAnnotation(element);
    } else {
      return new SafeWrapException[0];
    }
  }

  private SafeWrapException[] extractClassElementFromAnnotation(Element element) {
    Element builtinElement =
        processingEnvironment.getElementUtils().getTypeElement(MethodGenerator.wrapExceptionAnnotationClass.getCanonicalName());
    TypeMirror builtinType = builtinElement.asType();

    List<SafeWrapException> exceptionWrappers = new ArrayList<>();
    for (AnnotationMirror am : element.getAnnotationMirrors()) {
      if (am.getAnnotationType().equals(builtinType)) {
        TypeElement valueFrom = null;
        TypeElement valueTo = null;
        for (var entry : am.getElementValues().entrySet()) {
          Name key = entry.getKey().getSimpleName();
          var annotationVisitor = new AnnotationTypeVisitor();
          switch (key.toString()) {
            case FROM_ELEMENT_NAME -> valueFrom = entry.getValue().accept(annotationVisitor, null);
            case TO_ELEMENT_NAME -> valueTo = entry.getValue().accept(annotationVisitor, null);
          }
        }
        if (valueFrom != null) {
          exceptionWrappers.add(new SafeWrapException(valueFrom, Optional.ofNullable(valueTo)));
        }
      }
    }
    return exceptionWrappers.toArray(SafeWrapException[]::new);
  }

  private SafeWrapException[] extractClassElementFromAnnotationContainer(Element element) {
    Element builtinElement =
        processingEnvironment.getElementUtils().getTypeElement(
            MethodGenerator.wrapExceptionsAnnotationClass.getCanonicalName());
    Types tpeUtils = processingEnvironment.getTypeUtils();
    TypeMirror builtinType = builtinElement.asType();

    List<SafeWrapException> wrappedExceptions = new ArrayList<>();
    for (AnnotationMirror am : element.getAnnotationMirrors()) {
      if (tpeUtils.isSameType(am.getAnnotationType(), builtinType)) {
        for (var entry : am.getElementValues().entrySet()) {
          var anotVisitor = new AnnotationArrayVisitor();
          var collectedWrapExceptions = entry.getValue().accept(anotVisitor, null);
          wrappedExceptions.addAll(collectedWrapExceptions);
        }
      }
    }
    return wrappedExceptions.toArray(SafeWrapException[]::new);
  }

  private class AnnotationArrayVisitor extends SimpleAnnotationValueVisitor14<List<SafeWrapException>, Object> {
    private final List<SafeWrapException> elements = new ArrayList<>();
    private final AnnotationTypeVisitor typeVisitor = new AnnotationTypeVisitor();

    @Override
    public List<SafeWrapException> visitArray(List<? extends AnnotationValue> vals, Object o) {
      for (var annotationValue : vals) {
        annotationValue.accept(this, o);
      }
      return elements;
    }

    @Override
    public List<SafeWrapException> visitAnnotation(AnnotationMirror a, Object o) {
      TypeElement valueFrom = null;
      TypeElement valueTo = null;
      for (var entry : a.getElementValues().entrySet()) {
        var name = entry.getKey().getSimpleName().toString();
        switch (name) {
          case FROM_ELEMENT_NAME -> valueFrom = entry.getValue().accept(typeVisitor, null);
          case TO_ELEMENT_NAME -> valueTo = entry.getValue().accept(typeVisitor, null);
          default -> processingEnvironment.getMessager().printMessage(
              Kind.ERROR,
              "Unknown annotation element name: " + name);
        }
      }
      if (valueFrom != null) {
        var safeWrapException = new SafeWrapException(valueFrom, Optional.ofNullable(valueTo));
        elements.add(safeWrapException);
      }
      return elements;
    }
  }

  private class AnnotationTypeVisitor extends SimpleAnnotationValueVisitor14<TypeElement, Object> {
    @Override
    public TypeElement visitType(TypeMirror t, Object o) {
      var element = processingEnvironment.getTypeUtils().asElement(t);
      var elementVisitor = new SimpleElementVisitor14<TypeElement, Object>() {
        @Override
        public TypeElement visitType(TypeElement e, Object o) {
          return e;
        }
      };
      var typeElement = element.accept(elementVisitor, null);
      if (typeElement == null) {
        processingEnvironment.getMessager().printMessage(Kind.ERROR, "Cannot find type element for " + t);
      }
      return typeElement;
    }
  }
}
