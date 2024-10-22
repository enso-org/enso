package org.enso.runtime.parser.processor;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.SimpleElementVisitor14;
import javax.tools.JavaFileObject;
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
      var suc = processIrNode(irNodeElem);
      if (!suc) {
        return false;
      }
    }
    return true;
  }

  private boolean processIrNode(Element irNodeElem) {
    if (irNodeElem.getKind() != ElementKind.INTERFACE) {
      printError("IRNode annotation can only be applied to interfaces", irNodeElem);
      return false;
    }
    assert irNodeElem instanceof TypeElement;
    var irNodeTypeElem = (TypeElement) irNodeElem;
    if (!Utils.isSubtypeOfIR(irNodeTypeElem, processingEnv)) {
      printError("Interface annotated with @IRNode must be a subtype of IR interface", irNodeElem);
      return false;
    }
    var enclosingElem = irNodeElem.getEnclosingElement();
    if (enclosingElem != null && enclosingElem.getKind() != ElementKind.PACKAGE) {
      printError("Interface annotated with @IRNode must not be nested", irNodeElem);
      return false;
    }
    var nestedInterfaces = collectNestedInterfaces(irNodeTypeElem);
    var irNodeInterfaceName = irNodeTypeElem.getSimpleName().toString();
    var pkgName = packageName(irNodeTypeElem);
    var newClassName = irNodeInterfaceName + "Gen";
    String newBinaryName;
    if (!pkgName.isEmpty()) {
      newBinaryName = pkgName + "." + newClassName;
    } else {
      newBinaryName = newClassName;
    }

    JavaFileObject srcGen = null;
    try {
      srcGen = processingEnv.getFiler().createSourceFile(newBinaryName, irNodeElem);
    } catch (IOException e) {
      printError("Failed to create source file for IRNode", irNodeElem);
    }
    assert srcGen != null;

    String generatedCode;
    if (nestedInterfaces.isEmpty()) {
      var classGenerator = new IRNodeClassGenerator(processingEnv, irNodeTypeElem, newClassName);
      generatedCode = generateSingleNodeClass(classGenerator);
    } else {
      var nestedClassGenerators =
          nestedInterfaces.stream()
              .map(
                  iface -> {
                    var newNestedClassName = iface.getSimpleName().toString() + "Gen";
                    return new IRNodeClassGenerator(processingEnv, iface, newNestedClassName);
                  })
              .toList();
      generatedCode =
          generateMultipleNodeClasses(nestedClassGenerators, newClassName, irNodeInterfaceName);
    }

    try {
      try (var lineWriter = new PrintWriter(srcGen.openWriter())) {
        lineWriter.write(generatedCode);
      }
    } catch (IOException e) {
      printError("Failed to write to source file for IRNode", irNodeElem);
      return false;
    }
    return true;
  }

  private String packageName(Element elem) {
    var pkg = processingEnv.getElementUtils().getPackageOf(elem);
    return pkg.getQualifiedName().toString();
  }

  private void printError(String msg, Element elem) {
    Utils.printError(msg, elem, processingEnv.getMessager());
  }

  /**
   * Generates code for a single class that implements a single interface annotated with {@link
   * IRNode}.
   *
   * @return The generated code ready to be written to a {@code .java} source.
   */
  private static String generateSingleNodeClass(IRNodeClassGenerator irNodeClassGen) {
    var imports =
        irNodeClassGen.imports().stream().collect(Collectors.joining(System.lineSeparator()));
    var code =
        """
        $imports

        public final class $className implements $interfaceName {
          $classBody
        }
        """
            .replace("$imports", imports)
            .replace("$className", irNodeClassGen.getClassName())
            .replace("$interfaceName", irNodeClassGen.getInterfaceName())
            .replace("$classBody", irNodeClassGen.classBody());
    return code;
  }

  /**
   * Generates code for many inner classes. This is the case when an outer interface annotated with
   * {@link IRNode} contains many nested interfaces.
   *
   * @param nestedClassGenerators Class generators for all the nested interfaces.
   * @param newOuterClassName Name for the newly generate public outer class.
   * @param outerInterfaceName Name of the interface annotated by {@link IRNode}, that is, the outer
   *     interface for which we are generating multiple inner classes.
   * @return The generated code ready to be written to a {@code .java} source.
   */
  private static String generateMultipleNodeClasses(
      List<IRNodeClassGenerator> nestedClassGenerators,
      String newOuterClassName,
      String outerInterfaceName) {
    var imports =
        nestedClassGenerators.stream()
            .flatMap(gen -> gen.imports().stream())
            .collect(Collectors.joining(System.lineSeparator()));
    var sb = new StringBuilder();
    sb.append(imports);
    sb.append(System.lineSeparator());
    sb.append(System.lineSeparator());
    sb.append("public final class ")
        .append(newOuterClassName)
        .append(" {")
        .append(System.lineSeparator());
    sb.append(System.lineSeparator());
    sb.append("  ")
        .append("private ")
        .append(newOuterClassName)
        .append("() {}")
        .append(System.lineSeparator());
    sb.append(System.lineSeparator());
    for (var classGen : nestedClassGenerators) {
      sb.append("  public static final class ")
          .append(classGen.getClassName())
          .append(" implements ")
          .append(outerInterfaceName)
          .append(".")
          .append(classGen.getInterfaceName())
          .append(" {")
          .append(System.lineSeparator());
      sb.append(Utils.indent(classGen.classBody(), 2));
      sb.append("  }");
      sb.append(System.lineSeparator());
    }
    sb.append("}");
    sb.append(System.lineSeparator());
    return sb.toString();
  }

  private List<TypeElement> collectNestedInterfaces(TypeElement interfaceType) {
    var nestedTypes = new ArrayList<TypeElement>();
    var typeVisitor =
        new SimpleElementVisitor14<Void, Void>() {
          @Override
          protected Void defaultAction(Element e, Void unused) {
            for (var childElem : e.getEnclosedElements()) {
              childElem.accept(this, unused);
            }
            return null;
          }

          @Override
          public Void visitType(TypeElement e, Void unused) {
            if (e.getKind() == ElementKind.INTERFACE) {
              nestedTypes.add(e);
            }
            return super.visitType(e, unused);
          }
        };
    for (var enclosedElem : interfaceType.getEnclosedElements()) {
      enclosedElem.accept(typeVisitor, null);
    }
    return nestedTypes;
  }
}
