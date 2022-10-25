package org.enso.interpreter.dsl.builtins;

import com.google.common.base.CaseFormat;
import com.sun.tools.javac.code.Attribute;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.util.Pair;
import org.apache.commons.lang3.StringUtils;
import org.enso.interpreter.dsl.Builtin;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Types;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public abstract class MethodGenerator {
  protected final boolean isStatic;
  protected final boolean isConstructor;
  private final boolean convertToGuestValue;
  protected final TypeWithKind returnTpe;

  private static final WrapExceptionExtractor wrapExceptionsExtractor =
      new WrapExceptionExtractor(Builtin.WrapException.class, Builtin.WrapExceptions.class);

  protected SafeWrapException[] wrapExceptions(
      ProcessingEnvironment processingEnv, Element element) {
    return wrapExceptionsExtractor.extract(processingEnv, element);
  }

  public MethodGenerator(
      boolean isStatic,
      boolean isConstructor,
      boolean convertToGuestValue,
      TypeWithKind returnTpe) {
    this.isStatic = isStatic;
    this.isConstructor = isConstructor;
    this.convertToGuestValue = convertToGuestValue;
    this.returnTpe = returnTpe;
  }

  public abstract List<String> generate(
      ProcessingEnvironment processingEnv, String name, String owner);

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
              throw new RuntimeException(
                  "If intended, automatic conversion of value of type "
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
  protected MethodParameter fromVariableElementToMethodParameter(int i, VariableElement v) {
    String ensoName =
        CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, v.getSimpleName().toString());
    return new MethodParameter(
        i,
        ensoName,
        v.asType().toString(),
        v.getAnnotationMirrors().stream().map(am -> am.toString()).collect(Collectors.toList()));
  }

  protected abstract Optional<Integer> expandVararg(int paramsLen, int paramIndex);

  /**
   * Helper class that encapsulates retrieving the values of elements which type involves Class<?>.
   * Such elements' values cannot be retrieved by invoking the <element>() method. Instead one has
   * to go through mirrors. The logic has to deal with the following scenarios: - method with an
   * individual annotation - method with multiple annotations of the same type, thus implicitly
   * being annotation with container annotation - method with an explicit container annotation
   *
   * <p>Refer to <a
   * href="https://area-51.blog/2009/02/13/getting-class-values-from-annotations-in-an-annotationprocessor/">blog</a>
   * for details.
   */
  private static class WrapExceptionExtractor {

    private static final String FromElementName = "from";
    private static final String ToElementName = "to";
    private static final String ValueElementName = "value";

    private Class<? extends Annotation> wrapExceptionAnnotationClass;
    private Class<? extends Annotation> wrapExceptionsAnnotationClass;

    public WrapExceptionExtractor(
        Class<? extends Annotation> wrapExceptionAnnotationClass,
        Class<? extends Annotation> wrapExceptionsAnnotationClass) {
      this.wrapExceptionAnnotationClass = wrapExceptionAnnotationClass;
      this.wrapExceptionsAnnotationClass = wrapExceptionsAnnotationClass;
    }

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
    public SafeWrapException[] extract(ProcessingEnvironment processingEnv, Element element) {
      if (element.getAnnotation(wrapExceptionsAnnotationClass) != null) {
        return extractClassElementFromAnnotationContainer(
            processingEnv, element, wrapExceptionsAnnotationClass);
      } else if (element.getAnnotation(wrapExceptionAnnotationClass) != null) {
        return extractClassElementFromAnnotation(
            processingEnv, element, wrapExceptionAnnotationClass);
      } else {
        return new SafeWrapException[0];
      }
    }

    private SafeWrapException[] extractClassElementFromAnnotation(
        ProcessingEnvironment processingEnv, Element element, Class<?> annotationClass) {
      Element builtinElement =
          processingEnv.getElementUtils().getTypeElement(annotationClass.getCanonicalName());
      TypeMirror builtinType = builtinElement.asType();

      List<SafeWrapException> exceptionWrappers = new ArrayList<>();
      for (AnnotationMirror am : element.getAnnotationMirrors()) {
        if (am.getAnnotationType().equals(builtinType)) {
          Attribute.Class valueFrom = null;
          Attribute.Class valueTo = null;
          for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry :
              am.getElementValues().entrySet()) {
            Name key = entry.getKey().getSimpleName();
            if (key.toString().equals(FromElementName)) {
              valueFrom = (Attribute.Class) (entry.getValue());
            } else if (key.toString().equals(ToElementName)) {
              valueTo = (Attribute.Class) (entry.getValue());
            }
          }
          if (valueFrom != null && valueTo != null) {
            exceptionWrappers.add(new SafeWrapException(valueFrom, valueTo));
          }
        }
      }
      return exceptionWrappers.toArray(new SafeWrapException[0]);
    }

    private SafeWrapException[] extractClassElementFromAnnotationContainer(
        ProcessingEnvironment processingEnv, Element element, Class<?> annotationClass) {

      Element builtinElement =
          processingEnv.getElementUtils().getTypeElement(annotationClass.getCanonicalName());
      Types tpeUtils = processingEnv.getTypeUtils();
      TypeMirror builtinType = builtinElement.asType();

      List<SafeWrapException> wrappedExceptions = new ArrayList<>();
      for (AnnotationMirror am : element.getAnnotationMirrors()) {
        if (tpeUtils.isSameType(am.getAnnotationType(), builtinType)) {
          for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry :
              am.getElementValues().entrySet()) {
            if (ValueElementName.equals(entry.getKey().getSimpleName().toString())) {
              Attribute.Array wrapExceptions = (Attribute.Array) entry.getValue();
              for (int i = 0; i < wrapExceptions.values.length; i++) {
                Attribute.Class valueFrom = null;
                Attribute.Class valueTo = null;
                Attribute.Compound attr = (Attribute.Compound) wrapExceptions.values[i];
                for (Pair<Symbol.MethodSymbol, Attribute> p : attr.values) {
                  Name key = p.fst.getSimpleName();
                  if (key.contentEquals(FromElementName)) {
                    valueFrom = (Attribute.Class) p.snd;
                  } else if (key.contentEquals(ToElementName)) {
                    valueTo = (Attribute.Class) p.snd;
                  }
                }
                if (valueFrom != null && valueTo != null) {
                  SafeWrapException converted = new SafeWrapException(valueFrom, valueTo);
                  wrappedExceptions.add(converted);
                }
              }
              break;
            }
          }
        }
      }
      return wrappedExceptions.toArray(new SafeWrapException[0]);
    }
  }
}
