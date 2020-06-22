package org.enso.interpreter.dsl;

import com.google.auto.service.AutoService;
import org.enso.interpreter.dsl.model.MethodDefinition;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

@SupportedAnnotationTypes("org.enso.interpreter.dsl.BuiltinMethod")
@SupportedSourceVersion(SourceVersion.RELEASE_8)
@AutoService(Processor.class)
public class MethodProcessor extends AbstractProcessor {

  void print(Object o) {
    processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING, o.toString());
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      print(annotatedElements);
      for (Element elt : annotatedElements) {
        TypeElement element = (TypeElement) elt;
        ExecutableElement executeMethod =
            element.getEnclosedElements().stream()
                .filter(
                    x -> {
                      if (!(x instanceof ExecutableElement)) return false;
                      Name name = x.getSimpleName();
                      return name.contentEquals("execute");
                    })
                .map(x -> (ExecutableElement) x)
                .findFirst()
                .orElseGet(
                    () -> {
                      processingEnv
                          .getMessager()
                          .printMessage(Diagnostic.Kind.ERROR, "No execute method.");
                      return null;
                    });
        if (executeMethod == null) return true;
        BuiltinMethod ann = element.getAnnotation(BuiltinMethod.class);
        print(executeMethod);
        String pkgName =
            processingEnv.getElementUtils().getPackageOf(element).getQualifiedName().toString();
        String className = element.getSimpleName().toString();
        print(pkgName);
        print(className);
        try {
          generateCode(new MethodDefinition(pkgName, className, executeMethod, ann));
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }

    return true;
  }

  private final List<String> necessaryImports =
      Arrays.asList(
          "com.oracle.truffle.api.frame.VirtualFrame",
          "org.enso.interpreter.node.expression.builtin.BuiltinRootNode",
          "org.enso.interpreter.runtime.callable.argument.ArgumentDefinition",
          "com.oracle.truffle.api.nodes.UnexpectedResultException",
          "org.enso.interpreter.runtime.callable.function.Function",
          "org.enso.interpreter.runtime.callable.function.FunctionSchema",
          "org.enso.interpreter.runtime.state.Stateful",
          "org.enso.interpreter.runtime.error.TypeError",
          "org.enso.interpreter.runtime.type.TypesGen",
          "org.enso.interpreter.Language");

  private void generateCode(MethodDefinition methodDefinition) throws IOException {
    JavaFileObject gen =
        processingEnv.getFiler().createSourceFile(methodDefinition.getQualifiedName());

    methodDefinition.getArguments().forEach(this::print);

    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + methodDefinition.getPackageName() + ";");
      out.println();

      necessaryImports.forEach(pkg -> out.println("import " + pkg + ";"));

      out.println();

      out.println("public class " + methodDefinition.getClassName() + " extends BuiltinRootNode {");
      out.println("  private @Child " + methodDefinition.getOriginalClassName() + " bodyNode;");

      out.println();

      out.println("  private " + methodDefinition.getClassName() + "(Language language) {");
      out.println("    super(language);");
      out.println("    bodyNode = new " + methodDefinition.getOriginalClassName() + "();");
      out.println("  }");

      out.println();

      out.println("  public static Function makeFunction(Language language) {");
      out.println("    return Function.fromBuiltinRootNode(");
      out.println("        new " + methodDefinition.getClassName() + "(language),");
      out.println("        FunctionSchema.CallStrategy.ALWAYS_DIRECT,");
      List<String> argumentDefs = new ArrayList<>();
      for (int i = 0; i < methodDefinition.getArguments().size(); i++) {
        MethodDefinition.ArgumentDefinition arg = methodDefinition.getArguments().get(i);
        argumentDefs.add(
            "        new ArgumentDefinition("
                + i
                + ", \""
                + arg.getName()
                + "\", ArgumentDefinition.ExecutionMode.EXECUTE)");
      }
      out.println(String.join(",\n", argumentDefs) + ");");
      out.println("  }");

      out.println();

      out.println("  @Override");
      out.println("  public Stateful execute(VirtualFrame frame) {");
      out.println("    Object state = Function.ArgumentsHelper.getState(frame.getArguments());");
      out.println(
          "    Object[] arguments = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());");
      List<String> callArgNames = new ArrayList<>();
      for (int i = 0; i < methodDefinition.getArguments().size(); i++) {
        callArgNames.add("arg" + i);
        genArgRead(
            out,
            i,
            methodDefinition.getArguments().get(i),
            methodDefinition.getDeclaredName(),
            "arguments");
      }
      out.println(
          "    return new Stateful(state, bodyNode.execute("
              + String.join(", ", callArgNames)
              + "));");
      out.println("  }");

      out.println();

      out.println("  @Override");
      out.println("  public String getName() {");
      out.println("    return \"" + methodDefinition.getDeclaredName() + "\";");
      out.println("  }");

      out.println();

      out.println("}");
    }
  }

  private void genArgRead(
      PrintWriter out,
      int position,
      MethodDefinition.ArgumentDefinition arg,
      String methodName,
      String argsArray) {
    if (arg.getName().equals("this") && position == 0) {
      genUncheckedArgRead(out, position, arg, argsArray);
    } else {
      genCheckedArgRead(out, position, arg, methodName, argsArray);
    }
  }

  private void genUncheckedArgRead(
      PrintWriter out, int position, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String castName = "TypesGen.as" + capitalize(arg.getType());
    String varName = "arg" + position;
    out.println(
        "    "
            + arg.getType()
            + " "
            + varName
            + " = "
            + castName
            + "("
            + argsArray
            + "["
            + position
            + "]"
            + ");");
  }

  private void genCheckedArgRead(
      PrintWriter out,
      int position,
      MethodDefinition.ArgumentDefinition arg,
      String methodName,
      String argsArray) {
    String castName = "TypesGen.expect" + capitalize(arg.getType());
    String varName = "arg" + position;
    out.println("    " + arg.getType() + " " + varName + ";");
    out.println("    try {");
    out.println("      " + varName + " = " + castName + "(" + argsArray + "[" + position + "]);");
    out.println("    } catch (UnexpectedResultException e) {");
    out.println(
        "      throw new TypeError(\"Unexpected type provided for argument `"
            + arg.getName()
            + "` in "
            + methodName
            + "\", this);");
    out.println("    }");
  }

  private String capitalize(String name) {
    return name.substring(0, 1).toUpperCase() + name.substring(1);
  }
}
