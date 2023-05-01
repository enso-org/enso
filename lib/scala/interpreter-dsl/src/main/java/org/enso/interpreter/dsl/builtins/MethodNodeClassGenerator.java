package org.enso.interpreter.dsl.builtins;

import com.google.common.base.CaseFormat;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.*;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

import org.enso.interpreter.dsl.Builtin;

public abstract class MethodNodeClassGenerator {
  ClassName builtinNode;
  ClassName ownerClazz;
  ClassName stdlibOwner;

  public MethodNodeClassGenerator(
      ClassName builtinNode, ClassName ownerClazz, ClassName stdlibOwner) {
    this.builtinNode = builtinNode;
    this.ownerClazz = ownerClazz;
    this.stdlibOwner = stdlibOwner;
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
      String ownerMethodName,
      boolean isAutoRegister,
      Boolean needsFrame)
      throws IOException {
    JavaFileObject gen =
        processingEnv.getFiler().createSourceFile(builtinNode.jvmFriendlyFullyQualifiedName());
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
      String moduleOwnerInfo = "";
      if (!isAutoRegister) {
        moduleOwnerInfo = ", autoRegister = " + isAutoRegister;
      }
      if (needsFrame != null) {
        moduleOwnerInfo = moduleOwnerInfo + ", needsFrame = " + needsFrame;
      }
      out.println(
          "@BuiltinMethod(type = \""
              + ensoTypeName
              + "\", name = \""
              + ensoMethodName
              + "\", description = \"\"\"\n"
              + description
              + "\"\"\""
              + moduleOwnerInfo
              + ")");
      if (isAbstract()) {
        out.println("public abstract class " + builtinNode.jvmFriendlyName() + " extends Node {");
        out.println();

        out.println("  static " + builtinNode.jvmFriendlyName() + " build() {");
        out.println("    return " + builtinNode.jvmFriendlyName() + "Gen.create();");
        out.println("  }");
        out.println();
      } else {
        out.println("public class " + builtinNode.jvmFriendlyName() + " extends Node {");
        out.println();
      }
      for (String line : methodsGen().generate(processingEnv, ownerMethodName, ownerClazz.name())) {
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
   * @return a method generator for `execute` method and, potentially, specializations
   */
  protected abstract MethodGenerator methodsGen();

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
            + "\", description = \"\"\"\n"
            + description
            + "\"\"\")");
  }

  private static final List<String> methodNecessaryImports =
      Arrays.asList(
          "com.oracle.truffle.api.dsl.*",
          "com.oracle.truffle.api.library.CachedLibrary",
          "com.oracle.truffle.api.nodes.Node",
          "org.enso.interpreter.dsl.*",
          "org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode",
          "org.enso.interpreter.runtime.EnsoContext",
          "org.enso.interpreter.runtime.builtin.Builtins",
          "org.enso.interpreter.runtime.data.Array",
          "org.enso.interpreter.runtime.error.PanicException");
}
