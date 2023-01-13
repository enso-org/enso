package org.enso.interpreter.dsl.atom;

import org.openide.util.lookup.ServiceProvider;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@SupportedAnnotationTypes({
    "org.enso.interpreter.dsl.atom.LayoutSpec"
})
@ServiceProvider(service = Processor.class)
public class LayoutSpecProcessor extends AbstractProcessor {
  @Override
  public final boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      for (Element elt : annotatedElements) {
        if (elt.getKind() == ElementKind.CLASS) {
          try {
            processClass((TypeElement) elt);
          } catch (IOException ioe) {
            processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, ioe.getMessage());
          }
        } else {
          processingEnv
              .getMessager()
              .printMessage(
                  Diagnostic.Kind.ERROR,
                  "Invalid use of " + annotation.getSimpleName() + " with " + elt.getKind());
        }
      }
    }
    return true;
  }

  private void processClass(TypeElement element) throws IOException {
    var annotation = element.getAnnotation(LayoutSpec.class);
    var pkg = processingEnv.getElementUtils().getPackageOf(element).getQualifiedName().toString();
    var layoutName = element.getSimpleName().toString();
    writeAtoms(pkg, layoutName, annotation);
    writeFactory(pkg, layoutName, annotation);
  }

  private void writeAtoms(String pkg, String layoutName, LayoutSpec spec) throws IOException {
    for (int arity = spec.minFields(); arity <= spec.maxFields(); arity++) {
      for (int unbox = 0; unbox <= arity; unbox++) {
        writeAtom(pkg, layoutName, unbox, arity - unbox);
      }
    }
  }

  private String fieldName(int index) {
    return "field" + index;
  }

  private void writeAtom(String pkg, String layoutName, int countUnboxed, int countBoxed) throws IOException {
    var className = layoutName + "_Atom_" + countUnboxed + "_" + countBoxed;
    var file = processingEnv.getFiler().createSourceFile(pkg + "." + className);
    try (var out = new PrintWriter(file.openWriter())) {
      out.println("package " + pkg + ";");
      out.println("import org.enso.interpreter.runtime.callable.atom.Atom;");
      out.println("import org.enso.interpreter.runtime.callable.atom.AtomConstructor;");
      out.println("import com.oracle.truffle.api.dsl.NodeFactory;");
      out.println("import com.oracle.truffle.api.dsl.Specialization;");
      out.println();
      out.println("public class " + className + " extends UnboxingAtom {");
      writeFields(out, countUnboxed, countBoxed);
      writeConstructor(out, className, countUnboxed, countBoxed);
      for (int i = 0; i < countUnboxed; i++) {
        writeLongGetter(out, i, className);
        writeDoubleGetter(out, i, className);
      }
      for (int i = 0; i < countBoxed; i++) {
        writeObjectGetter(out, i + countUnboxed, className);
      }
      writeInstantiator(out, className, countUnboxed, countBoxed);
      out.println("}");
    }
  }

  private void writeInstantiator(PrintWriter out, String className, int countUnboxed, int countBoxed) {
    out.println("  public abstract static class InstantiatorNode extends UnboxingAtom.InstantiatorNode {");
    out.println("    @Specialization");
    out.println("    Atom doExecute(AtomConstructor constructor, Layout layout, Object[] args) {");
    var args = new String[countUnboxed + countBoxed + 2];
    args[0] = "constructor";
    args[1] = "layout";
    for (int i = 0; i < countUnboxed; i++) {
      args[i + 2] = "(long) args[" + i + "]";
    }
    for (int i = 0; i < countBoxed; i++) {
      args[i + 2 + countUnboxed] = "args[" + (i + countUnboxed) + "]";
    }
    out.println("      return new " + className + "(" + String.join(", ", args) + ");");
    out.println("    }");

    out.println("  }");

    out.println();

    out.println("  public static NodeFactory<InstantiatorNode> getInstantiatorNodeFactory() {");
    out.println("    return " + className + "Factory.InstantiatorNodeFactory.getInstance();");
    out.println("  }");
  }

  private void writeLongGetter(PrintWriter out, int index, String className) {
    out.println("  public static abstract class FieldGetter_" + index + "L_Node extends FieldGetterNode {");
    out.println("    @Specialization");
    out.println("    long doAtom(" + className + " atom) {");
    out.println("      return atom." + fieldName(index) + ";");
    out.println("    }");
    out.println("  }");
    out.println();
  }

  private void writeDoubleGetter(PrintWriter out, int index, String className) {
    out.println("  public static abstract class FieldGetter_" + index + "D_Node extends FieldGetterNode {");
    out.println("    @Specialization");
    out.println("    double doAtom(" + className + " atom) {");
    out.println("      return Double.longBitsToDouble(atom." + fieldName(index) + ");");
    out.println("    }");
    out.println("  }");
    out.println();
  }

  private void writeObjectGetter(PrintWriter out, int index, String className) {
    out.println("  public static abstract class FieldGetter_" + index + "_Node extends FieldGetterNode {");
    out.println("    @Specialization");
    out.println("    Object doAtom(" + className + " atom) {");
    out.println("      return atom." + fieldName(index) + ";");
    out.println("    }");
    out.println("  }");
    out.println();
  }

  private void writeFields(PrintWriter out, int countUnboxed, int countBoxed) {
    for (int i = 0; i < countUnboxed; i++) {
      out.println("  private long " + fieldName(i) + ";");
    }
    for (int i = 0; i < countBoxed; i++) {
      out.println("  private Object " + fieldName(i + countUnboxed) + ";");
    }
    out.println();
  }

  private void writeConstructor(PrintWriter out, String className, int countUnboxed, int countBoxed) {
    var consArgs = new String[countUnboxed + countBoxed + 2];
    consArgs[0] = "AtomConstructor constructor";
    consArgs[1] = "Layout layout";
    for (int i = 0; i < countUnboxed; i++) {
      consArgs[i + 2] = "long " + fieldName(i);
    }
    for (int i = 0; i < countBoxed; i++) {
      consArgs[i + 2 + countUnboxed] = "Object " + fieldName(i + countUnboxed);
    }
    out.println("  public " + className + "(" + String.join(", ", consArgs) + ") {");
    out.println("    super(constructor, layout);");
    for (int i = 0; i < countUnboxed; i++) {
      out.println("    this." + fieldName(i) + " = " + fieldName(i) + ";");
    }
    for (int i = 0; i < countBoxed; i++) {
      out.println("    this." + fieldName(i + countUnboxed) + " = " + fieldName(i + countUnboxed) + ";");
    }
    out.println("  }");
    out.println();
  }

  private void writeFactory(String pkg, String layoutName, LayoutSpec spec) throws IOException {
    var name = layoutName + "Factory";
    var file = processingEnv.getFiler().createSourceFile(pkg + "." + name);
    try (var out = new PrintWriter(file.openWriter())) {
      out.println("package " + pkg + ";");
      out.println("public class " + name + " {");
      out.println("}");
    }
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }
}
