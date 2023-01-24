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

@SupportedAnnotationTypes({"org.enso.interpreter.dsl.atom.LayoutSpec"})
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

  private String atomClassName(String layout, int countUnboxed, int countBoxed) {
    return layout + "_Atom_" + countUnboxed + "_" + countBoxed;
  }

  private void writeAtom(String pkg, String layoutName, int countUnboxed, int countBoxed)
      throws IOException {
    var className = atomClassName(layoutName, countUnboxed, countBoxed);
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
        writeSetter(out, i + countUnboxed, className);
      }
      writeFieldGetterFactoryGetter(out, className, countUnboxed, countBoxed);
      writeFieldSetterFactoryGetter(out, className, countUnboxed, countBoxed);
      writeInstantiator(out, className, countUnboxed, countBoxed);
      out.println("}");
    }
  }

  private void writeFieldGetterFactoryGetter(
      PrintWriter out, String className, int countUnboxed, int countBoxed) {
    out.println(
        "  public static NodeFactory<? extends FieldGetterNode> getFieldGetterNodeFactory(int storageIndex, boolean isDoubleIfUnboxed) {");
    out.println("    return switch (storageIndex) {");
    for (int i = 0; i < countUnboxed; i++) {
      out.println("      case " + i + " -> isDoubleIfUnboxed ?");
      out.println(
          "        " + className + "Factory.FieldGetter_" + i + "D_NodeFactory.getInstance() :");
      out.println(
          "        " + className + "Factory.FieldGetter_" + i + "L_NodeFactory.getInstance();");
    }
    for (int i = countUnboxed; i < countBoxed + countUnboxed; i++) {
      out.println(
          "      case "
              + i
              + " -> "
              + className
              + "Factory.FieldGetter_"
              + i
              + "_NodeFactory.getInstance();");
    }
    out.println("      default -> throw new IllegalArgumentException(\"Invalid storage index\");");
    out.println("    };");
    out.println("  }");
  }

  private void writeFieldSetterFactoryGetter(
      PrintWriter out, String className, int countUnboxed, int countBoxed) {
    out.println(
        "  public static NodeFactory<? extends FieldSetterNode> getFieldSetterNodeFactory(int storageIndex) {");
    out.println("    return switch (storageIndex) {");
    for (int i = countUnboxed; i < countBoxed + countUnboxed; i++) {
      out.println(
          "      case "
              + i
              + " -> "
              + className
              + "Factory.FieldSetter_"
              + i
              + "_NodeFactory.getInstance();");
    }
    out.println("      default -> null;");
    out.println("    };");
    out.println("  }");
  }

  private void writeInstantiator(
      PrintWriter out, String className, int countUnboxed, int countBoxed) {
    out.println(
        "  public abstract static class InstantiatorNode extends UnboxingAtom.InstantiatorNode {");
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
    out.println(
        "  public static abstract class FieldGetter_" + index + "L_Node extends FieldGetterNode {");
    out.println("    @Specialization");
    out.println("    long doAtom(" + className + " atom) {");
    out.println("      return atom." + fieldName(index) + ";");
    out.println("    }");
    out.println("  }");
    out.println();
  }

  private void writeDoubleGetter(PrintWriter out, int index, String className) {
    out.println(
        "  public static abstract class FieldGetter_" + index + "D_Node extends FieldGetterNode {");
    out.println("    @Specialization");
    out.println("    double doAtom(" + className + " atom) {");
    out.println("      return Double.longBitsToDouble(atom." + fieldName(index) + ");");
    out.println("    }");
    out.println("  }");
    out.println();
  }

  private void writeObjectGetter(PrintWriter out, int index, String className) {
    out.println(
        "  public static abstract class FieldGetter_" + index + "_Node extends FieldGetterNode {");
    out.println("    @Specialization");
    out.println("    Object doAtom(" + className + " atom) {");
    out.println("      return atom." + fieldName(index) + ";");
    out.println("    }");
    out.println("  }");
    out.println();
  }

  private void writeSetter(PrintWriter out, int index, String className) {
    out.println(
        "  public static abstract class FieldSetter_" + index + "_Node extends FieldSetterNode {");
    out.println("    @Specialization");
    out.println("    void doAtom(" + className + " atom, Object value) {");
    out.println("      atom." + fieldName(index) + "= value;");
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

  private void writeConstructor(
      PrintWriter out, String className, int countUnboxed, int countBoxed) {
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
      out.println(
          "    this." + fieldName(i + countUnboxed) + " = " + fieldName(i + countUnboxed) + ";");
    }
    out.println("  }");
    out.println();
  }

  private void writeFactory(String pkg, String layoutName, LayoutSpec spec) throws IOException {
    var name = layoutName + "Factory";
    var file = processingEnv.getFiler().createSourceFile(pkg + "." + name);
    try (var out = new PrintWriter(file.openWriter())) {
      out.println("package " + pkg + ";");
      out.println();
      out.println("import com.oracle.truffle.api.dsl.NodeFactory;");
      out.println();
      out.println("public class " + name + " {");
      writeGetterFactory(out, layoutName, spec);
      writeSetterFactory(out, layoutName, spec);
      writeInstantiatorFactory(out, layoutName, spec);
      out.println("}");
    }
  }

  private void writeGetterFactory(PrintWriter out, String layoutName, LayoutSpec spec) {
    out.println(
        "  public static NodeFactory<UnboxingAtom.FieldGetterNode>[] getFieldGetterNodeFactories(int numDoubles, int numLongs, int numBoxed) {");
    out.println("    var arity = numDoubles + numLongs + numBoxed;");
    out.println("    var numUnboxed = numDoubles + numLongs;");
    out.println("    var result = new NodeFactory[arity];");
    out.println("    switch (numUnboxed) {");
    for (int unboxedCase = 0; unboxedCase <= spec.maxFields(); unboxedCase++) {
      out.println("      case " + unboxedCase + ":");
      out.println("        switch (numBoxed) {");
      for (int boxedCase = Math.max(spec.minFields() - unboxedCase, 0);
          boxedCase + unboxedCase <= spec.maxFields();
          boxedCase++) {
        out.println("          case " + boxedCase + ":");
        out.println("            for (int i = 0; i < numDoubles; i++) {");
        out.println(
            "              result[i] = "
                + atomClassName(layoutName, unboxedCase, boxedCase)
                + ".getFieldGetterNodeFactory(i, true);");
        out.println("            }");
        out.println("            for (int i = numDoubles; i < numUnboxed; i++) {");
        out.println(
            "              result[i] = "
                + atomClassName(layoutName, unboxedCase, boxedCase)
                + ".getFieldGetterNodeFactory(i, false);");
        out.println("            }");
        out.println("            for (int i = numUnboxed; i < arity; i++) {");
        out.println(
            "              result[i] = "
                + atomClassName(layoutName, unboxedCase, boxedCase)
                + ".getFieldGetterNodeFactory(i, false);");
        out.println("            }");
        out.println("            break;");
      }
      out.println("        }");
      out.println("        break;");
    }
    out.println("    }");
    out.println("    return result;");
    out.println("  }");
    out.println();
  }

  private void writeSetterFactory(PrintWriter out, String layoutName, LayoutSpec spec) {
    out.println(
        "  public static NodeFactory<UnboxingAtom.FieldSetterNode>[] getFieldSetterNodeFactories(int numDoubles, int numLongs, int numBoxed) {");
    out.println("    var arity = numDoubles + numLongs + numBoxed;");
    out.println("    var numUnboxed = numDoubles + numLongs;");
    out.println("    var result = new NodeFactory[arity];");
    out.println("    switch (numUnboxed) {");
    for (int unboxedCase = 0; unboxedCase <= spec.maxFields(); unboxedCase++) {
      out.println("      case " + unboxedCase + ":");
      out.println("        switch (numBoxed) {");
      for (int boxedCase = Math.max(spec.minFields() - unboxedCase, 0);
          boxedCase + unboxedCase <= spec.maxFields();
          boxedCase++) {
        out.println("          case " + boxedCase + ":");
        out.println("            for (int i = numUnboxed; i < arity; i++) {");
        out.println(
            "              result[i] = "
                + atomClassName(layoutName, unboxedCase, boxedCase)
                + ".getFieldSetterNodeFactory(i);");
        out.println("            }");
        out.println("            break;");
      }
      out.println("        }");
      out.println("        break;");
    }
    out.println("    }");
    out.println("    return result;");
    out.println("  }");
    out.println();
  }

  private void writeInstantiatorFactory(PrintWriter out, String layoutName, LayoutSpec spec) {
    out.println(
        "  public static NodeFactory<? extends UnboxingAtom.InstantiatorNode> getInstantiatorNodeFactory(int numUnboxed, int numBoxed) {");
    out.println("    return switch (numUnboxed) {");
    for (int unboxedCase = 0; unboxedCase <= spec.maxFields(); unboxedCase++) {
      out.println("      case " + unboxedCase + " -> switch (numBoxed) {");
      for (int boxedCase = Math.max(spec.minFields() - unboxedCase, 0);
          boxedCase + unboxedCase <= spec.maxFields();
          boxedCase++) {
        out.println(
            "        case "
                + boxedCase
                + " -> "
                + atomClassName(layoutName, unboxedCase, boxedCase)
                + ".getInstantiatorNodeFactory();");
      }
      out.println("        default -> throw new IllegalArgumentException(\"Unsupported arity\");");
      out.println("      };");
    }
    out.println("      default -> throw new IllegalArgumentException(\"Unsupported arity\");");
    out.println("    };");
    out.println("  }");
    out.println();
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }
}
