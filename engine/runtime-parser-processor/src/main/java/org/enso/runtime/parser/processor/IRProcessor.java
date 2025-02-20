package org.enso.runtime.parser.processor;

import java.io.IOException;
import java.io.PrintWriter;
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
    var generateIRElems = roundEnv.getElementsAnnotatedWith(GenerateIR.class);
    for (var generateIRElem : generateIRElems) {
      try {
        ensureIsClass(generateIRElem);
        processGenerateIRElem((TypeElement) generateIRElem);
      } catch (IRProcessingException e) {
        Element element;
        if (e.getElement() != null) {
          element = e.getElement();
        } else {
          element = generateIRElem;
        }
        processingEnv.getMessager().printMessage(Kind.ERROR, e.getMessage(), element);
        return false;
      }
    }
    return true;
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
      throw new IRProcessingException(
          "Class annotated with @GenerateIR must have exactly one constructor annotated with"
              + " @GenerateFields",
          clazz);
    }
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
            .map(TypeElement::getSimpleName)
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
