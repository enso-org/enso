package org.enso.interpreter.dsl.model;

import org.enso.interpreter.dsl.BuiltinMethod;

import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;
import java.util.ArrayList;
import java.util.List;

public class MethodDefinition {
  private final String packageName;
  private final String originalClassName;
  private final String className;
  private final String qualifiedName;
  private final BuiltinMethod annotation;
  private final List<ArgumentDefinition> arguments;

  public MethodDefinition(
      String packageName,
      String originalClassName,
      ExecutableElement execute,
      BuiltinMethod annotation) {
    this.packageName = packageName;
    this.originalClassName = originalClassName;
    this.className = generateClassName(originalClassName);
    this.qualifiedName = packageName + "." + className;
    this.annotation = annotation;
    this.arguments = initArguments(execute);
  }

  private String generateClassName(String originalClassName) {
    String baseName =
        originalClassName.endsWith("Node")
            ? originalClassName.substring(0, originalClassName.length() - 4)
            : originalClassName;
    return baseName + "MethodGen";
  }

  private List<ArgumentDefinition> initArguments(ExecutableElement method) {
    List<ArgumentDefinition> args = new ArrayList<>();
    List<? extends VariableElement> params = method.getParameters();
    for (VariableElement param : params) {
      String originalName = param.getSimpleName().toString();
      String name = originalName.equals("self") ? "this" : originalName;
      args.add(new ArgumentDefinition(param.asType(), name));
    }
    return args;
  }

  public String getPackageName() {
    return packageName;
  }

  public String getOriginalClassName() {
    return originalClassName;
  }

  public String getClassName() {
    return className;
  }

  public String getQualifiedName() {
    return qualifiedName;
  }

  public String getDeclaredName() {
    return annotation.type() + "." + annotation.name();
  }

  public List<ArgumentDefinition> getArguments() {
    return arguments;
  }

  public static class ArgumentDefinition {
    private final String type;
    private final String name;

    public ArgumentDefinition(TypeMirror type, String name) {
      String[] typeNameSegments = type.toString().split("\\.");
      this.type = typeNameSegments[typeNameSegments.length - 1];
      this.name = name;
    }

    public String getType() {
      return type;
    }

    public String getName() {
      return name;
    }

    @Override
    public String toString() {
      return "ArgumentDefinition{" + "type='" + type + '\'' + ", name='" + name + '\'' + '}';
    }
  }
}
