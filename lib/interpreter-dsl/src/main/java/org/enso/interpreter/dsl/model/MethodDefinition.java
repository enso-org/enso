package org.enso.interpreter.dsl.model;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

import javax.lang.model.element.*;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import java.util.*;

public class MethodDefinition {
  private static final String STATEFUL = "org.enso.interpreter.runtime.state.Stateful";

  private final String packageName;
  private final String originalClassName;
  private final String className;
  private final String qualifiedName;
  private final BuiltinMethod annotation;
  private final List<ArgumentDefinition> arguments;
  private final Set<String> imports;
  private final boolean modifiesState;
  private final boolean needsCallerInfo;
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
    this.needsCallerInfo = arguments.stream().anyMatch(ArgumentDefinition::isCallerInfo);
    this.modifiesState = execute.getReturnType().toString().equals(STATEFUL);
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
      ArgumentDefinition def = new ArgumentDefinition(param, position);
      args.add(def);
      if (def.isPositional()) {
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

  public boolean needsCallerInfo() {
    return needsCallerInfo;
  }

  public boolean isAlwaysDirect() {
    return annotation.alwaysDirect();
  }

  public String getConstructorExpression() {
    return constructorExpression;
  }

  public static class ArgumentDefinition {
    private static final String VIRTUAL_FRAME = "com.oracle.truffle.api.frame.VirtualFrame";
    private static final String OBJECT = "java.lang.Object";
    private static final String THUNK = "org.enso.interpreter.runtime.callable.argument.Thunk";
    private static final String CALLER_INFO = "org.enso.interpreter.runtime.callable.CallerInfo";
    private final String typeName;
    private final TypeMirror type;
    private final String name;
    private final boolean isState;
    private final boolean isFrame;
    private final boolean isCallerInfo;
    private final int position;

    public ArgumentDefinition(VariableElement element, int position) {
      type = element.asType();
      String[] typeNameSegments = type.toString().split("\\.");
      typeName = typeNameSegments[typeNameSegments.length - 1];
      String originalName = element.getSimpleName().toString();
      name = originalName.equals("self") ? "this" : originalName;
      isState = element.getAnnotation(MonadicState.class) != null && type.toString().equals(OBJECT);
      isFrame = type.toString().equals(VIRTUAL_FRAME);
      isCallerInfo = type.toString().equals(CALLER_INFO);
      this.position = position;
    }

    public boolean isState() {
      return isState;
    }

    public boolean isFrame() {
      return isFrame;
    }

    public boolean isCallerInfo() {
      return isCallerInfo;
    }

    public boolean isPositional() {
      return !isFrame() && !isState() && !isCallerInfo();
    }

    public int getPosition() {
      return position;
    }

    public Optional<String> getImport() {
      if (type.getKind() == TypeKind.DECLARED) {
        if (!type.toString().equals(OBJECT)) {
          return Optional.of(type.toString());
        }
      }
      return Optional.empty();
    }

    public boolean requiresCast() {
      return !type.toString().equals(OBJECT);
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
      return type.toString().equals(THUNK);
    }
  }
}
