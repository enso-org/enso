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
          generateCode(new MethodDefinition(pkgName, className, element, executeMethod, ann));
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

    for (MethodDefinition.ArgumentDefinition def : methodDefinition.getArguments()) {
      print(def.toString() + " " + def.getType().getKind());
    }

    Set<String> allImports = new HashSet<>(necessaryImports);
    allImports.addAll(methodDefinition.getImports());

    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + methodDefinition.getPackageName() + ";");
      out.println();

      allImports.forEach(pkg -> out.println("import " + pkg + ";"));

      out.println();

      out.println("public class " + methodDefinition.getClassName() + " extends BuiltinRootNode {");
      out.println("  private @Child " + methodDefinition.getOriginalClassName() + " bodyNode;");

      out.println();

      out.println("  private " + methodDefinition.getClassName() + "(Language language) {");
      out.println("    super(language);");
      out.println("    bodyNode = " + methodDefinition.getConstructorExpression() + ";");
      out.println("  }");

      out.println();

      out.println("  public static Function makeFunction(Language language) {");
      out.println("    return Function.fromBuiltinRootNode(");
      out.println("        new " + methodDefinition.getClassName() + "(language),");
      if (methodDefinition.isAlwaysDirect()) {
        out.println("        FunctionSchema.CallStrategy.ALWAYS_DIRECT,");
      } else {
        out.println("        FunctionSchema.CallStrategy.DIRECT_WHEN_TAIL,");
      }
      List<String> argumentDefs = new ArrayList<>();
      for (MethodDefinition.ArgumentDefinition arg : methodDefinition.getArguments()) {
        if (!arg.isState()) {
          String executionMode = arg.isSuspended() ? "PASS_THUNK" : "EXECUTE";
          argumentDefs.add(
              "        new ArgumentDefinition("
                  + arg.getPosition()
                  + ", \""
                  + arg.getName()
                  + "\", ArgumentDefinition.ExecutionMode."
                  + executionMode
                  + ")");
        }
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
      for (MethodDefinition.ArgumentDefinition argumentDefinition :
          methodDefinition.getArguments()) {
        if (!argumentDefinition.isState()) {
          callArgNames.add("arg" + argumentDefinition.getPosition());
          genArgRead(out, argumentDefinition, methodDefinition.getDeclaredName(), "arguments");
        } else {
          callArgNames.add("state");
        }
      }
      String executeCall = "bodyNode.execute(" + String.join(", ", callArgNames) + ")";
      if (methodDefinition.modifiesState()) {
        out.println("    return " + executeCall + ";");
      } else {
        out.println("    return new Stateful(state, " + executeCall + ");");
      }
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
      MethodDefinition.ArgumentDefinition arg,
      String methodName,
      String argsArray) {
    if (!arg.requiresCast()) {
      genUncastedArgRead(out, arg, argsArray);
    } else if (arg.getName().equals("this") && arg.getPosition() == 0) {
      genUncheckedArgRead(out, arg, argsArray);
    } else {
      genCheckedArgRead(out, arg, methodName, argsArray);
    }
  }

  private void genUncastedArgRead(
      PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String varName = "arg" + arg.getPosition();
    out.println(
        "    "
            + arg.getTypeName()
            + " "
            + varName
            + " = "
            + argsArray
            + "["
            + arg.getPosition()
            + "];");
  }

  private void genUncheckedArgRead(
      PrintWriter out, MethodDefinition.ArgumentDefinition arg, String argsArray) {
    String castName = "TypesGen.as" + capitalize(arg.getTypeName());
    String varName = "arg" + arg.getPosition();
    out.println(
        "    "
            + arg.getTypeName()
            + " "
            + varName
            + " = "
            + castName
            + "("
            + argsArray
            + "["
            + arg.getPosition()
            + "]);");
  }

  private void genCheckedArgRead(
      PrintWriter out,
      MethodDefinition.ArgumentDefinition arg,
      String methodName,
      String argsArray) {
    String castName = "TypesGen.expect" + capitalize(arg.getTypeName());
    String varName = "arg" + arg.getPosition();
    out.println("    " + arg.getTypeName() + " " + varName + ";");
    out.println("    try {");
    out.println(
        "      " + varName + " = " + castName + "(" + argsArray + "[" + arg.getPosition() + "]);");
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
