package org.enso.runtime.parser.processor;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import javax.tools.Diagnostic.Kind;
import javax.tools.JavaFileObject;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.processor.utils.DependencySorter;
import org.enso.runtime.parser.processor.utils.DependencySorter.CyclicDependencyException;
import org.enso.runtime.parser.processor.utils.Utils;

@SupportedAnnotationTypes({
  "org.enso.runtime.parser.dsl.GenerateIR",
  "org.enso.runtime.parser.dsl.IRChild",
  "org.enso.runtime.parser.dsl.IRCopyMethod",
})
public class IRProcessor extends AbstractProcessor {

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    if (annotations.isEmpty()) {
      return false;
    }
    // Elements sorted in processing order.
    List<TypeElement> orderedElems;
    try {
      var generateIRElems =
          roundEnv.getElementsAnnotatedWith(GenerateIR.class).stream()
              .map(
                  elem -> {
                    ensureIsClass(elem);
                    return (TypeElement) elem;
                  })
              .collect(Collectors.toUnmodifiableSet());
      orderedElems = orderByReferences(generateIRElems);
    } catch (IRProcessingException e) {
      if (e.getElement() == null) {
        processingEnv.getMessager().printMessage(Kind.ERROR, e.getMessage());
      } else {
        processingEnv.getMessager().printMessage(Kind.ERROR, e.getMessage(), e.getElement());
      }
      return false;
    }
    for (var elemToProcess : orderedElems) {
      try {
        processGenerateIRElem(elemToProcess);
      } catch (IRProcessingException e) {
        Element element;
        if (e.getElement() != null) {
          element = e.getElement();
        } else {
          element = elemToProcess;
        }
        processingEnv.getMessager().printMessage(Kind.ERROR, e.getMessage(), element);
        return false;
      }
    }
    return true;
  }

  /**
   * There might be some references between the annotated classes inside this compilation unit. If
   * that is the case, it means that they must be ordered so that the first processed class is the
   * one that does not depend on any other class. Otherwise, there might be some compilation errors
   * due to missing super classes.
   *
   * <p>If a cyclic dependency is detected, {@link IRProcessingException} is thrown.
   *
   * <p>If there are no dependencies between the annotated classes inside this compilation unit,
   * they are returned in an arbitrary order.
   *
   * <h2>Example</h2>
   *
   * An example of a problematic case is:
   *
   * <pre>
   *   &#64;GenerateIR
   *   class A extends AGen {
   *     &#64;GenerateFields
   *     A(&#64;IRChild B b) {...}
   *   }
   *
   *   &#64;GenerateIR
   *   class B extends BGen {}
   * </pre>
   *
   * <p>In this case, {@code A} <emph>depends on</emph> {@code B}, so we need to ensure that {@code
   * B} is processed first (a super class is generated for it first).
   *
   * @return List of classes ordered by their references. The list has the same size as the input
   *     set.
   */
  private List<TypeElement> orderByReferences(Set<TypeElement> classesToProcess) {
    var classesToProcessNames =
        classesToProcess.stream().map(type -> type.getSimpleName().toString()).toList();
    var classesToProcessMap =
        classesToProcess.stream()
            .collect(Collectors.toMap(tp -> tp.getQualifiedName().toString(), tp -> tp));
    var dependencies = new HashMap<String, Set<String>>();
    for (var clazz : classesToProcess) {
      var annotatedCtors =
          clazz.getEnclosedElements().stream()
              .filter(elem -> Utils.hasAnnotation(elem, GenerateFields.class))
              .toList();
      if (annotatedCtors.size() != 1) {
        throw singleAnnotatedCtorError(clazz);
      }
      var annotatedCtor = annotatedCtors.get(0);
      if (annotatedCtor.getKind() != ElementKind.CONSTRUCTOR) {
        throw new IRProcessingException(
            "Only constructors can be annotated with GenerateFields", annotatedCtor);
      }
      var ctor = (ExecutableElement) annotatedCtor;
      var childTypeNames =
          ctor.getParameters().stream()
              .filter(param -> Utils.hasAnnotation(param, IRChild.class))
              .map(
                  param -> {
                    var paramType = param.asType();
                    var paramTypeElem = processingEnv.getTypeUtils().asElement(paramType);
                    if (paramTypeElem == null) {
                      throw new IRProcessingException(
                          "Cannot find element for type " + paramType, null);
                    }
                    if (paramTypeElem instanceof TypeElement typeElem) {
                      return typeElem.getQualifiedName().toString();
                    } else {
                      throw new IRProcessingException(
                          "Parameter type is not a TypeElement: " + paramTypeElem, paramTypeElem);
                    }
                  })
              .toList();
      for (var childTypeName : childTypeNames) {
        if (classesToProcessNames.contains(childTypeName)) {
          var clazzName = clazz.getQualifiedName().toString();
          var deps = dependencies.computeIfAbsent(clazzName, k -> new HashSet<>());
          deps.add(childTypeName);
        }
      }
    }
    if (dependencies.isEmpty()) {
      // If dependencies are empty, it means there are no internal dependencies,
      // in that case, just return the classes in any order.
      return classesToProcess.stream().toList();
    }
    try {
      DependencySorter.ensureNoCycles(dependencies);
    } catch (CyclicDependencyException e) {
      throw new IRProcessingException("Cyclic dependency detected: " + e.getMessage(), null, e);
    }
    var sortedDeps = DependencySorter.topologicalSort(dependencies);
    // Map class names to their TypeElements
    var sortedDepTypes =
        sortedDeps.stream()
            .map(
                depName -> {
                  var depClazz =
                      classesToProcess.stream()
                          .filter(clazz -> clazz.getSimpleName().toString().equals(depName))
                          .findFirst()
                          .orElseThrow(
                              () -> new IRProcessingException("Class not found: " + depName, null));
                  return depClazz;
                })
            .collect(Collectors.toCollection(ArrayList::new));

    // Append the rest of the classesToProcess to the sortedDepTypes
    for (var entry : classesToProcessMap.entrySet()) {
      var fqn = entry.getKey();
      var tp = entry.getValue();
      // Poor man's solution. Let's hope the number of classes to process is small.
      var isInSortedDeps =
          sortedDepTypes.stream()
              .anyMatch(depTp -> depTp.getQualifiedName().toString().equals(fqn));
      if (!isInSortedDeps) {
        sortedDepTypes.add(tp);
      }
    }
    if (sortedDepTypes.size() != classesToProcess.size()) {
      throw new IRProcessingException(
          "orderByReferences failure: "
              + "sortedDepTypes: "
              + sortedDepTypes
              + ", classesToProcess: "
              + classesToProcessNames,
          null);
    }
    return sortedDepTypes;
  }

  /**
   * @param processedClassElem Class annotated with {@link GenerateIR}.
   */
  private void processGenerateIRElem(TypeElement processedClassElem) {
    ensureIsPublicFinal(processedClassElem);
    ensureEnclosedInInterfaceOrPackage(processedClassElem);
    ensureHasSingleAnnotatedConstructor(processedClassElem);
    ensureExtendsGeneratedSuperclass(processedClassElem);

    var processedClass = constructProcessedClass(processedClassElem);
    var pkgName = packageName(processedClassElem);
    var newClassName = generatedClassName(processedClassElem);
    String newBinaryName;
    if (!pkgName.isEmpty()) {
      newBinaryName = pkgName + "." + newClassName;
    } else {
      newBinaryName = newClassName;
    }

    JavaFileObject srcGen;
    try {
      srcGen = processingEnv.getFiler().createSourceFile(newBinaryName, processedClassElem);
    } catch (IOException e) {
      throw new IRProcessingException(
          "Failed to create source file for IRNode", processedClassElem, e);
    }

    String generatedCode;
    var classGenerator = new IRNodeClassGenerator(processingEnv, processedClass, newClassName);
    generatedCode = generateSingleNodeClass(classGenerator, processedClass, pkgName);

    try {
      try (var lineWriter = new PrintWriter(srcGen.openWriter())) {
        lineWriter.write(generatedCode);
      }
    } catch (IOException e) {
      throw new IRProcessingException(
          "Failed to write to source file for IRNode", processedClassElem, e);
    }
  }

  private String generatedClassName(TypeElement processedClassElem) {
    var superClass = processedClassElem.getSuperclass();
    if (superClass.getKind() == TypeKind.ERROR) {
      // The super class does not yet exist
      return superClass.toString();
    } else if (superClass.getKind() == TypeKind.DECLARED) {
      var superClassElem = (TypeElement) processingEnv.getTypeUtils().asElement(superClass);
      return superClassElem.getSimpleName().toString();
    } else {
      throw new IRProcessingException(
          "Super class must be a declared type",
          processingEnv.getTypeUtils().asElement(superClass));
    }
  }

  private ProcessedClass constructProcessedClass(TypeElement processedClassElem) {
    // GenerateIR.interfaces cannot be accessed directly, we have to access the
    // classes via type mirrors.
    TypeElement irIfaceToImplement = Utils.irTypeElement(processingEnv);
    List<TypeElement> allInterfacesToImplement = List.of();
    for (var annotMirror : processedClassElem.getAnnotationMirrors()) {
      if (annotMirror.getAnnotationType().toString().equals(GenerateIR.class.getName())) {
        var annotMirrorElemValues =
            processingEnv.getElementUtils().getElementValuesWithDefaults(annotMirror);
        for (var entry : annotMirrorElemValues.entrySet()) {
          if (entry.getKey().getSimpleName().toString().equals("interfaces")) {
            var annotValueVisitor = new GenerateIRAnnotationVisitor(processingEnv, entry.getKey());
            entry.getValue().accept(annotValueVisitor, null);
            if (annotValueVisitor.getIrInterface() != null) {
              irIfaceToImplement = annotValueVisitor.getIrInterface();
            }
            allInterfacesToImplement = annotValueVisitor.getAllInterfaces();
          }
        }
      }
    }
    Utils.hardAssert(irIfaceToImplement != null);
    if (!Utils.isSubtypeOfIR(irIfaceToImplement, processingEnv)) {
      throw new IRProcessingException(
          "Interface to implement must be a subtype of IR interface", irIfaceToImplement);
    }
    var annotatedCtor = getAnnotatedCtor(processedClassElem);
    var processedClass =
        new ProcessedClass(
            processedClassElem, annotatedCtor, irIfaceToImplement, allInterfacesToImplement);
    return processedClass;
  }

  private void ensureIsClass(Element elem) {
    if (elem.getKind() != ElementKind.CLASS) {
      throw new IRProcessingException("GenerateIR annotation can only be applied to classes", elem);
    }
  }

  private void ensureIsPublicFinal(TypeElement clazz) {
    if (!clazz.getModifiers().contains(Modifier.FINAL)
        || !clazz.getModifiers().contains(Modifier.PUBLIC)) {
      throw new IRProcessingException(
          "Class annotated with @GenerateIR must be public final", clazz);
    }
  }

  private void ensureEnclosedInInterfaceOrPackage(TypeElement clazz) {
    var enclosingElem = clazz.getEnclosingElement();
    if (enclosingElem != null) {
      if (!(enclosingElem.getKind() == ElementKind.PACKAGE
          || enclosingElem.getKind() == ElementKind.INTERFACE)) {
        throw new IRProcessingException(
            "Class annotated with @GenerateIR must be enclosed in a package or an interface",
            clazz);
      }
    }
  }

  private void ensureHasSingleAnnotatedConstructor(TypeElement clazz) {
    var annotatedCtorsCnt =
        clazz.getEnclosedElements().stream()
            .filter(elem -> elem.getKind() == ElementKind.CONSTRUCTOR)
            .filter(ctor -> ctor.getAnnotation(GenerateFields.class) != null)
            .count();
    if (annotatedCtorsCnt != 1) {
      throw singleAnnotatedCtorError(clazz);
    }
  }

  private static IRProcessingException singleAnnotatedCtorError(TypeElement clazz) {
    return new IRProcessingException(
        "Class annotated with @GenerateIR must have exactly one constructor annotated with"
            + " @GenerateFields",
        clazz);
  }

  private void ensureExtendsGeneratedSuperclass(TypeElement clazz) {
    var superClass = clazz.getSuperclass();
    if (superClass.getKind() == TypeKind.NONE || superClass.toString().equals("java.lang.Object")) {
      throw new IRProcessingException(
          "Class annotated with @GenerateIR must have 'extends' clause", clazz);
    }
  }

  private static ExecutableElement getAnnotatedCtor(TypeElement clazz) {
    // It should already be ensured that there is only a single annotated constructor in the class,
    // hence the AssertionError
    return clazz.getEnclosedElements().stream()
        .filter(elem -> elem.getAnnotation(GenerateFields.class) != null)
        .map(elem -> (ExecutableElement) elem)
        .findFirst()
        .orElseThrow(
            () -> new IRProcessingException("No constructor annotated with GenerateFields", clazz));
  }

  private String packageName(Element elem) {
    var pkg = processingEnv.getElementUtils().getPackageOf(elem);
    return pkg.getQualifiedName().toString();
  }

  /**
   * Generates code for a super class.
   *
   * @param pkgName Package of the current processed class.
   * @return The generated code ready to be written to a {@code .java} source.
   */
  private static String generateSingleNodeClass(
      IRNodeClassGenerator irNodeClassGen, ProcessedClass processedClass, String pkgName) {
    var imports =
        irNodeClassGen.imports().stream()
            .sorted()
            .collect(Collectors.joining(System.lineSeparator()));
    var pkg = pkgName.isEmpty() ? "" : "package " + pkgName + ";";
    var interfaces =
        processedClass.getInterfaces().stream()
            .map(TypeElement::getQualifiedName)
            .collect(Collectors.joining(", "));
    var code =
        """
        $pkg

        $imports

        $docs
        abstract class $className implements $interfaces {
          $classBody
        }
        """
            .replace("$pkg", pkg)
            .replace("$imports", imports)
            .replace("$docs", jdoc(processedClass))
            .replace("$className", irNodeClassGen.getClassName())
            .replace("$interfaces", interfaces)
            .replace("$classBody", irNodeClassGen.classBody());
    return code;
  }

  private static String jdoc(ProcessedClass processedClass) {
    var thisClassName = IRProcessor.class.getName();
    var processedClassName = processedClass.getClazz().getQualifiedName().toString();
    var docs =
        """
        /**
         * Generated by {@code $thisClassName} IR annotation processor.
         * Generated from {@link $processedClassName}.
         * The {@link $processedClassName} is meant to extend this generated class.
         */
        """
            .replace("$thisClassName", thisClassName)
            .replace("$processedClassName", processedClassName);
    return docs;
  }
}
