package org.enso.interpreter.dsl.model;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

import javax.lang.model.element.*;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import java.util.*;

public class MethodDefinition {
  private final String packageName;
  private final String originalClassName;
  private final String className;
  private final String qualifiedName;
  private final BuiltinMethod annotation;
  private final List<ArgumentDefinition> arguments;
  private final Set<String> imports;
  private final boolean modifiesState;
  private final String constructorExpression;

  public MethodDefinition(
      String packageName,
      String originalClassName,
      TypeElement element,
      ExecutableElement execute,
      BuiltinMethod annotation) {
    this.packageName = packageName;
    this.originalClassName = originalClassName;
    this.className = generateClassName(originalClassName);
    this.qualifiedName = packageName + "." + className;
    this.annotation = annotation;
    this.arguments = initArguments(execute);
    this.imports = initImports();
    this.modifiesState =
        execute.getReturnType().toString().equals("org.enso.interpreter.runtime.state.Stateful");
    this.constructorExpression = initConstructor(element);
  }

  private String initConstructor(TypeElement element) {
    boolean useBuild =
        element.getEnclosedElements().stream()
            .anyMatch(
                el -> {
                  if (el.getKind() != ElementKind.METHOD) {
                    return false;
                  }
                  ExecutableElement method = (ExecutableElement) el;
                  return method.getSimpleName().contentEquals("build")
                      && method.getParameters().isEmpty()
                      && method.getModifiers().contains(Modifier.STATIC);
                });
    if (useBuild) {
      return originalClassName + ".build()";
    } else {
      return "new " + originalClassName + "()";
    }
  }

  private Set<String> initImports() {
    Set<String> result = new HashSet<>();
    for (ArgumentDefinition arg : arguments) {
      Optional<String> imp = arg.getImport();
      imp.ifPresent(result::add);
    }
    return result;
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
    int position = 0;
    for (VariableElement param : params) {
      String originalName = param.getSimpleName().toString();
      String name = originalName.equals("self") ? "this" : originalName;
      boolean isState = param.getAnnotation(MonadicState.class) != null;
      args.add(new ArgumentDefinition(param.asType(), name, isState, position));
      if (!isState) {
        position++;
      }
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

  public Set<String> getImports() {
    return imports;
  }

  public boolean modifiesState() {
    return modifiesState;
  }

  public boolean isAlwaysDirect() {
    return annotation.alwaysDirect();
  }

  public String getConstructorExpression() {
    return constructorExpression;
  }

  public static class ArgumentDefinition {
    private final String typeName;
    private final TypeMirror type;
    private final String name;
    private final boolean isState;
    private final int position;

    public ArgumentDefinition(TypeMirror type, String name, boolean isState, int position) {
      String[] typeNameSegments = type.toString().split("\\.");
      this.typeName = typeNameSegments[typeNameSegments.length - 1];
      this.type = type;
      this.name = name;
      this.isState = isState;
      this.position = position;
    }

    public boolean isState() {
      return isState;
    }

    public int getPosition() {
      return position;
    }

    public Optional<String> getImport() {
      if (type.getKind() == TypeKind.DECLARED) {
        if (!type.toString().equals("java.lang.Object")) {
          return Optional.of(type.toString());
        }
      }
      return Optional.empty();
    }

    public boolean requiresCast() {
      return !type.toString().equals("java.lang.Object");
    }

    public String getTypeName() {
      return typeName;
    }

    public TypeMirror getType() {
      return type;
    }

    public String getName() {
      return name;
    }

    public boolean isSuspended() {
      return type.toString().equals("org.enso.interpreter.runtime.callable.argument.Thunk");
    }

    @Override
    public String toString() {
      return "ArgumentDefinition{" + "type='" + typeName + '\'' + ", name='" + name + '\'' + '}';
    }
  }
}
