package org.enso.runtime.parser.processor;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
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
    if (!isSubtypeOfIR(irNodeElem.asType())) {
      printError("Interface annotated with @IRNode must be a subtype of IR interface", irNodeElem);
      return false;
    }
    var enclosingElem = irNodeElem.getEnclosingElement();
    if (enclosingElem != null && enclosingElem.getKind() != ElementKind.PACKAGE) {
      printError("Interface annotated with @IRNode must not be nested", irNodeElem);
      return false;
    }
    assert irNodeElem instanceof TypeElement;
    var irNodeTypeElem = (TypeElement) irNodeElem;
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
    var irNodeClassGen = new IRNodeClassGenerator(processingEnv, irNodeTypeElem, newClassName);
    try {
      try (var lineWriter = new PrintWriter(srcGen.openWriter())) {
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
                .replace("$className", newClassName)
                .replace("$interfaceName", irNodeInterfaceName)
                .replace("$classBody", irNodeClassGen.classBody());
        lineWriter.println(code);
        lineWriter.println();
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

  private boolean isSubtypeOfIR(TypeMirror type) {
    return Utils.isSubtypeOfIR(type, processingEnv);
  }

  private void printError(String msg, Element elem) {
    Utils.printError(msg, elem, processingEnv.getMessager());
  }
}
