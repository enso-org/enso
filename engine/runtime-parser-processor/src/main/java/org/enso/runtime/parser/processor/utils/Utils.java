package org.enso.runtime.parser.processor.utils;

import java.lang.annotation.Annotation;
import java.util.ArrayDeque;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.processor.GenerateIRAnnotationVisitor;
import org.enso.runtime.parser.processor.IRProcessingException;

public final class Utils {

  private static final String MAP_EXPRESSIONS = "mapExpressions";
  private static final String DUPLICATE = "duplicate";
  private static final String IR_INTERFACE_SIMPLE_NAME = "IR";
  private static final String IR_INTERFACE_FQN = "org.enso.compiler.core.IR";
  private static final String EXPRESSION_FQN = "org.enso.compiler.core.ir.Expression";
  private static final String SCALA_LIST = "scala.collection.immutable.List";
  private static final String SCALA_OPTION = "scala.Option";
  private static final String PERSISTANCE_REFERENCE = "org.enso.persist.Persistance.Reference";
  private static final String DIAGNOSTIC_STORAGE_FQN =
      "org.enso.compiler.core.ir.DiagnosticStorage";
  private static final String IDENTIFIED_LOCATION_FQN =
      "org.enso.compiler.core.ir.IdentifiedLocation";
  private static final String METADATA_STORAGE_FQN = "org.enso.compiler.core.ir.MetadataStorage";
  private static final String UUID_FQN = "java.util.UUID";

  private Utils() {}

  /** Returns true if the given {@code type} is a subtype of {@code org.enso.compiler.core.IR}. */
  public static boolean isSubtypeOfIR(TypeElement type, ProcessingEnvironment processingEnv) {
    if (isAnnotatedAndNotYetCompiled(type)) {
      return true;
    }
    var irIfaceFound =
        iterateSuperInterfaces(
            type,
            processingEnv,
            (TypeElement iface) -> {
              // current.getQualifiedName().toString() returns only "IR" as well, so we can't use
              // it.
              // This is because runtime-parser-processor project does not depend on runtime-parser
              // and
              // so the org.enso.compiler.core.IR interface is not available in the classpath.
              if (iface.getSimpleName().toString().equals(IR_INTERFACE_SIMPLE_NAME)) {
                return true;
              }
              return null;
            });
    return irIfaceFound != null;
  }

  /**
   * Returns true if the given type is annotated with {@link
   * org.enso.runtime.parser.dsl.GenerateIR}, and has no super class compiled yet. This can happen
   * if the currently processed compilation unit contains more than one class annotated with {@link
   * GenerateIR}.
   *
   * @param type
   * @return
   */
  private static boolean isAnnotatedAndNotYetCompiled(TypeElement type) {
    var anot = type.getAnnotation(GenerateIR.class);
    var superClass = type.getSuperclass();
    var superClassDoesNotExist = superClass.getKind() == TypeKind.ERROR;
    return anot != null && superClassDoesNotExist;
  }

  public static TypeElement irTypeElement(ProcessingEnvironment procEnv) {
    var ret = procEnv.getElementUtils().getTypeElement(IR_INTERFACE_FQN);
    hardAssert(
        ret != null,
        "Cannot resolve " + IR_INTERFACE_FQN + " from proc environment. Maybe missing import?");
    return ret;
  }

  public static TypeElement diagnosticStorageTypeElement(ProcessingEnvironment procEnv) {
    var ret = procEnv.getElementUtils().getTypeElement(DIAGNOSTIC_STORAGE_FQN);
    hardAssert(
        ret != null,
        "Cannot resolve "
            + DIAGNOSTIC_STORAGE_FQN
            + " from proc environment. Maybe missing import?");
    return ret;
  }

  public static TypeElement identifiedLocationTypeElement(ProcessingEnvironment procEnv) {
    var ret = procEnv.getElementUtils().getTypeElement(IDENTIFIED_LOCATION_FQN);
    hardAssert(
        ret != null,
        "Cannot resolve "
            + IDENTIFIED_LOCATION_FQN
            + " from proc environment. Maybe missing import?");
    return ret;
  }

  public static TypeElement metadataStorageTypeElement(ProcessingEnvironment procEnv) {
    var ret = procEnv.getElementUtils().getTypeElement(METADATA_STORAGE_FQN);
    hardAssert(
        ret != null,
        "Cannot resolve " + METADATA_STORAGE_FQN + " from proc environment. Maybe missing import?");
    return ret;
  }

  public static TypeElement uuidTypeElement(ProcessingEnvironment procEnv) {
    var ret = procEnv.getElementUtils().getTypeElement(UUID_FQN);
    hardAssert(
        ret != null,
        "Cannot resolve " + UUID_FQN + " from proc environment. Maybe missing import?");
    return ret;
  }

  /** Returns true if the given type extends {@code org.enso.compiler.core.ir.Expression} */
  public static boolean isSubtypeOfExpression(
      TypeMirror type, ProcessingEnvironment processingEnv) {
    var exprType = expressionType(processingEnv).asType();
    return processingEnv.getTypeUtils().isAssignable(type, exprType);
  }

  public static TypeElement expressionType(ProcessingEnvironment procEnv) {
    return procEnv.getElementUtils().getTypeElement(EXPRESSION_FQN);
  }

  /**
   * Returns the qualified type name of the given {@code typeMirror}. For generic types, the type
   * arguments are included, e.g., {@code java.util.List<java.lang.String>}.
   */
  public static String qualifiedTypeName(TypeMirror typeMirror) {
    if (typeMirror.getKind() == TypeKind.DECLARED) {
      var declared = (DeclaredType) typeMirror;
      var typeArgs = declared.getTypeArguments();
      var typeElem = (TypeElement) declared.asElement();
      if (!typeArgs.isEmpty()) {
        var typeArgsStr =
            typeArgs.stream().map(Utils::qualifiedTypeName).collect(Collectors.joining(", "));
        return typeElem.getQualifiedName() + "<" + typeArgsStr + ">";
      } else {
        return typeElem.getQualifiedName().toString();
      }
    }
    return typeMirror.toString();
  }

  public static String qualifiedTypeName(Element element) {
    if (element instanceof TypeElement typeElem) {
      return typeElem.getQualifiedName().toString();
    } else {
      return element.getSimpleName().toString();
    }
  }

  public static String indent(String code, int indentation) {
    return code.lines()
        .map(line -> " ".repeat(indentation) + line)
        .collect(Collectors.joining(System.lineSeparator()));
  }

  public static String indent(String code) {
    return indent(code, 2);
  }

  /**
   * Returns null if the given {@code typeMirror} is not a declared type and thus has no associated
   * {@link TypeElement}.
   */
  public static TypeElement typeMirrorToElement(TypeMirror typeMirror) {
    if (typeMirror.getKind() == TypeKind.DECLARED) {
      var elem = ((DeclaredType) typeMirror).asElement();
      if (elem instanceof TypeElement typeElem) {
        return typeElem;
      }
    }
    return null;
  }

  public static boolean isScalaOption(TypeMirror type, ProcessingEnvironment procEnv) {
    var elem = procEnv.getTypeUtils().asElement(type);
    if (elem instanceof TypeElement typeElem) {
      var optionType = procEnv.getElementUtils().getTypeElement(SCALA_OPTION);
      return procEnv.getTypeUtils().isSameType(optionType.asType(), typeElem.asType());
    }
    return false;
  }

  public static boolean isScalaList(TypeMirror type, ProcessingEnvironment procEnv) {
    var elem = procEnv.getTypeUtils().asElement(type);
    if (elem instanceof TypeElement typeElem) {
      var listType = procEnv.getElementUtils().getTypeElement(SCALA_LIST);
      return procEnv.getTypeUtils().isSameType(listType.asType(), typeElem.asType());
    }
    return false;
  }

  public static TypeMirror getTypeArgument(TypeMirror type) {
    if (type instanceof DeclaredType declaredType) {
      Utils.hardAssert(declaredType.getTypeArguments().size() == 1);
      return declaredType.getTypeArguments().get(0);
    }
    return null;
  }

  public static boolean isPersistanceReference(TypeMirror type, ProcessingEnvironment procEnv) {
    var elem = procEnv.getTypeUtils().asElement(type);
    if (elem instanceof TypeElement typeElem) {
      var listType = procEnv.getElementUtils().getTypeElement(PERSISTANCE_REFERENCE);
      return procEnv.getTypeUtils().isSameType(listType.asType(), typeElem.asType());
    }
    return false;
  }

  /**
   * Returns {@link GenerateIR#interfaces()} field as a list of {@link TypeElement}. Note that we
   * cannot access that field directly.
   *
   * @param annotatedClazz
   * @return
   */
  private static List<TypeElement> getGenerateIRInterfacesField(
      TypeElement annotatedClazz, ProcessingEnvironment processingEnv) {
    hardAssert(hasAnnotation(annotatedClazz, GenerateIR.class));
    List<TypeElement> allInterfacesToImplement = List.of();
    for (var annotMirror : annotatedClazz.getAnnotationMirrors()) {
      if (annotMirror.getAnnotationType().toString().equals(GenerateIR.class.getName())) {
        var annotMirrorElemValues =
            processingEnv.getElementUtils().getElementValuesWithDefaults(annotMirror);
        for (var entry : annotMirrorElemValues.entrySet()) {
          if (entry.getKey().getSimpleName().toString().equals("interfaces")) {
            var annotValueVisitor = new GenerateIRAnnotationVisitor(processingEnv, entry.getKey());
            entry.getValue().accept(annotValueVisitor, null);
            allInterfacesToImplement = annotValueVisitor.getAllInterfaces();
          }
        }
      }
    }
    return allInterfacesToImplement;
  }

  /**
   * Finds a method in the interface hierarchy. The interface hierarchy processing starts from
   * {@code interfaceType} and iterates until {@code org.enso.compiler.core.IR} interface type is
   * encountered. Every method in the hierarchy is checked by {@code methodPredicate}.
   *
   * @param interfaceType Type of the interface. Must extend {@code org.enso.compiler.core.IR}.
   * @param procEnv
   * @param methodPredicate Predicate that is called for each method in the hierarchy.
   * @return Method that satisfies the predicate or null if no such method is found.
   */
  public static ExecutableElement findMethod(
      TypeElement interfaceType,
      ProcessingEnvironment procEnv,
      Predicate<ExecutableElement> methodPredicate) {
    if (isAnnotatedAndNotYetCompiled(interfaceType)) {
      var ifaces = getGenerateIRInterfacesField(interfaceType, procEnv);
      for (var iface : ifaces) {
        var method = findMethod(iface, procEnv, methodPredicate);
        if (method != null) {
          return method;
        }
      }
      return null;
    } else {
      var foundMethod =
          iterateSuperInterfaces(
              interfaceType,
              procEnv,
              (TypeElement superInterface) -> {
                for (var enclosedElem : superInterface.getEnclosedElements()) {
                  if (enclosedElem instanceof ExecutableElement execElem) {
                    if (methodPredicate.test(execElem)) {
                      return execElem;
                    }
                  }
                }
                return null;
              });
      return foundMethod;
    }
  }

  /**
   * Find any override of {@link org.enso.compiler.core.IR#duplicate(boolean, boolean, boolean,
   * boolean) duplicate method}. Or the duplicate method on the interface itself. Note that there
   * can be an override with a different return type in a sub interface.
   *
   * @param interfaceType Interface from where the search is started. All super interfaces are
   *     searched transitively.
   * @return not null.
   */
  public static ExecutableElement findDuplicateMethod(
      TypeElement interfaceType, ProcessingEnvironment procEnv) {
    var duplicateMethod = findMethod(interfaceType, procEnv, Utils::isDuplicateMethod);
    hardAssert(
        duplicateMethod != null,
        "Interface "
            + interfaceType.getQualifiedName()
            + " must implement IR, so it must declare duplicate method");
    return duplicateMethod;
  }

  public static ExecutableElement findMapExpressionsMethod(
      TypeElement interfaceType, ProcessingEnvironment processingEnv) {
    var mapExprsMethod =
        findMethod(
            interfaceType,
            processingEnv,
            method -> method.getSimpleName().toString().equals(MAP_EXPRESSIONS));
    hardAssert(
        mapExprsMethod != null,
        "mapExpressions method must be found it must be defined at least on IR super interface");
    return mapExprsMethod;
  }

  public static void hardAssert(boolean condition) {
    hardAssert(condition, "Assertion failed");
  }

  public static void hardAssert(boolean condition, String msg) {
    if (!condition) {
      throw new IRProcessingException(msg, null);
    }
  }

  public static boolean hasNoAnnotations(Element element) {
    return element.getAnnotationMirrors().isEmpty();
  }

  public static boolean hasAnnotation(
      Element element, Class<? extends Annotation> annotationClass) {
    return element.getAnnotation(annotationClass) != null;
  }

  private static boolean isDuplicateMethod(ExecutableElement executableElement) {
    return executableElement.getSimpleName().toString().equals(DUPLICATE)
        && executableElement.getParameters().size() == 4;
  }

  /**
   * @param type Type from which the iterations starts.
   * @param processingEnv
   * @param ifaceVisitor Visitor that is called for each interface.
   * @param <T>
   */
  public static <T> T iterateSuperInterfaces(
      TypeElement type,
      ProcessingEnvironment processingEnv,
      InterfaceHierarchyVisitor<T> ifaceVisitor) {
    var interfacesToProcess = new ArrayDeque<TypeElement>();
    interfacesToProcess.add(type);
    while (!interfacesToProcess.isEmpty()) {
      var current = interfacesToProcess.pop();
      var iterationResult = ifaceVisitor.visitInterface(current);
      if (iterationResult != null) {
        return iterationResult;
      }
      // Add all super interfaces to the queue
      for (var superInterface : InterfaceCollector.collect(current, processingEnv.getTypeUtils())) {
        var superInterfaceElem = processingEnv.getTypeUtils().asElement(superInterface);
        if (superInterfaceElem instanceof TypeElement superInterfaceTypeElem) {
          interfacesToProcess.add(superInterfaceTypeElem);
        }
      }
    }
    return null;
  }

  public static <T> List<T> diff(List<T> superset, List<T> subset) {
    return superset.stream().filter(e -> !subset.contains(e)).collect(Collectors.toList());
  }
}
