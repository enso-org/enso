package org.enso.interpreter.dsl.builtins;

import com.sun.tools.javac.code.Attribute;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.util.Pair;
import org.enso.interpreter.dsl.Builtin;
import com.google.common.base.CaseFormat;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Types;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public abstract class MethodNodeClassGenerator {
  ClassName builtinNode;
  ClassName ownerClazz;
  ClassName stdlibOwner;
  Map<String, Integer> builtinTypesParamCount;

  private static final WrapExceptionExtractor wrapExceptionsExtractor =
      new WrapExceptionExtractor(Builtin.WrapException.class, Builtin.WrapExceptions.class);

  public MethodNodeClassGenerator(
      ClassName builtinNode,
      ClassName ownerClazz,
      ClassName stdlibOwner,
      Map<String, Integer> builtinTypesParamCount) {
    this.builtinNode = builtinNode;
    this.ownerClazz = ownerClazz;
    this.stdlibOwner = stdlibOwner;
    this.builtinTypesParamCount = builtinTypesParamCount;
  }

  protected SafeWrapException[] wrapExceptions(
      ProcessingEnvironment processingEnv, Element element) {
    return wrapExceptionsExtractor.extract(processingEnv, element);
  }

  /**
   * Checks if the method has been marked require explicit to guest value translations
   *
   * @param origin builtin method
   * @return true if the annotation exists, false otherwise
   */
  protected boolean needsGuestValueConversion(Element origin) {
    return origin.getAnnotation(Builtin.ReturningGuestObject.class) != null;
  }

  public void generate(
      ProcessingEnvironment processingEnv,
      String methodName,
      String description,
      String ownerMethodName)
      throws IOException {
    JavaFileObject gen =
        processingEnv.getFiler().createSourceFile(builtinNode.fullyQualifiedName());
    ;
    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      String ensoMethodName = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, methodName);
      String ensoTypeName = stdlibOwner.name().replaceAll("([a-z])([A-Z])", "$1_$2");
      out.println("package " + builtinNode.pkg() + ";");
      out.println();
      for (String importPkg : methodNecessaryImports) {
        out.println("import " + importPkg + ";");
      }
      out.println("import " + ownerClazz.fullyQualifiedName() + ";");
      out.println();
      out.println(
          "@BuiltinMethod(type = \""
              + ensoTypeName
              + "\", name = \""
              + ensoMethodName
              + "\", description = \""
              + description
              + "\")");
      if (isAbstract()) {
        out.println("public abstract class " + builtinNode.name() + " extends Node {");
        out.println();

        out.println("  static " + builtinNode.name() + " build() {");
        out.println("    return " + builtinNode.name() + "Gen.create();");
        out.println("  }");
        out.println();
      } else {
        out.println("public class " + builtinNode.name() + " extends Node {");
        out.println();
      }
      for (String line :
          methodsGen(processingEnv)
              .generate(ownerMethodName, ownerClazz.name(), builtinTypesParamCount)) {
        out.println("  " + line);
      }
      out.println();
      out.println("}");
      out.println();
    }
  }

  /**
   * Returns a method(s) generator for the given node class.
   *
   * @param processingEnv Current annotation processing environment
   * @return a method generator for `execute` method and, potentially, specializations
   */
  protected abstract MethodGenerator methodsGen(ProcessingEnvironment processingEnv);

  /**
   * Determines if the class should be concrete or abstract.
   *
   * @return true if the method node should be abstract, false if a concrete class should be
   *     generated
   */
  protected abstract boolean isAbstract();

  /**
   * Generate package, imports and @BuiltinMethod for a target class
   *
   * @param out output stream where code will be written to
   * @param methodName name of the annotated method (lower camel-case)
   * @param description description of the builtin method
   */
  private void generateClassHeader(PrintWriter out, String methodName, String description) {
    String ensoMethodName = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, methodName);
    String ensoTypeName = stdlibOwner.name().replaceAll("([a-z])([A-Z])", "$1_$2");
    out.println("package " + builtinNode.pkg() + ";");
    out.println();
    for (String importPkg : methodNecessaryImports) {
      out.println("import " + importPkg + ";");
    }
    out.println("import " + ownerClazz.fullyQualifiedName() + ";");
    out.println();
    out.println(
        "@BuiltinMethod(type = \""
            + ensoTypeName
            + "\", name = \""
            + ensoMethodName
            + "\", description = \""
            + description
            + "\")");
  }

  private static final List<String> methodNecessaryImports =
      Arrays.asList(
          "com.oracle.truffle.api.dsl.Cached",
          "com.oracle.truffle.api.dsl.Specialization",
          "com.oracle.truffle.api.nodes.Node",
          "org.enso.interpreter.dsl.BuiltinMethod",
          "org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode",
          "org.enso.interpreter.runtime.Context",
          "org.enso.interpreter.runtime.builtin.Builtins",
          "org.enso.interpreter.runtime.data.Array",
          "org.enso.interpreter.runtime.error.PanicException");

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
    private static final String PropagateElementName = "propagate";
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
          Attribute.Constant valuePropagate = null;
          for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry :
              am.getElementValues().entrySet()) {
            Name key = entry.getKey().getSimpleName();
            if (key.toString().equals(FromElementName)) {
              valueFrom = (Attribute.Class) (entry.getValue());
            } else if (key.toString().equals(ToElementName)) {
              valueTo = (Attribute.Class) (entry.getValue());
            } else if (key.toString().equals(PropagateElementName)) {
              valuePropagate = (Attribute.Constant) (entry.getValue());
            }
          }
          if (valueFrom != null && valueTo != null) {
            exceptionWrappers.add(new SafeWrapException(valueFrom, valueTo, valuePropagate));
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
                Attribute.Constant valuePropagate = null;
                Attribute.Compound attr = (Attribute.Compound) wrapExceptions.values[i];
                for (Pair<Symbol.MethodSymbol, Attribute> p : attr.values) {
                  Name key = p.fst.getSimpleName();
                  if (key.contentEquals(FromElementName)) {
                    valueFrom = (Attribute.Class) p.snd;
                  } else if (key.contentEquals(ToElementName)) {
                    valueTo = (Attribute.Class) p.snd;
                  } else if (key.contentEquals(PropagateElementName)) {
                    valuePropagate = (Attribute.Constant) p.snd;
                  }
                }
                if (valueFrom != null && valueTo != null) {
                  SafeWrapException converted =
                      new SafeWrapException(valueFrom, valueTo, valuePropagate);
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
