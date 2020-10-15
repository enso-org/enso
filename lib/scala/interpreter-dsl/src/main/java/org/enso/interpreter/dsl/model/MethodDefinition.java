package org.enso.interpreter.dsl.model;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic;
import java.util.*;

/** A domain-specific representation of a builtin method. */
public class MethodDefinition {
  private static final String STATEFUL = "org.enso.interpreter.runtime.state.Stateful";

  private final String packageName;
  private final String originalClassName;
  private final String className;
  private final String qualifiedName;
  private final BuiltinMethod annotation;
  private final TypeElement element;
  private final ExecutableElement executeMethod;
  private final List<ArgumentDefinition> arguments;
  private final Set<String> imports;
  private final boolean modifiesState;
  private final boolean needsCallerInfo;
  private final String constructorExpression;

  /**
   * Creates a new instance of this class.
   *
   * @param packageName the name of the package this method is declared in.
   * @param element the element (class) declaring this method.
   * @param execute the element (method) containing the logic.
   */
  public MethodDefinition(String packageName, TypeElement element, ExecutableElement execute) {
    this.annotation = element.getAnnotation(BuiltinMethod.class);
    this.element = element;
    this.executeMethod = execute;
    this.originalClassName = element.getSimpleName().toString();
    this.packageName = packageName;
    this.className = generateClassName(originalClassName);
    this.qualifiedName = packageName + "." + className;
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

  /**
   * Checks the validity of this definition with respect to required properties.
   *
   * <p>Any invalid elements will be reported as errors.
   *
   * @param processingEnvironment the current processing environment.
   * @return whether the definition is fully valid.
   */
  public boolean validate(ProcessingEnvironment processingEnvironment) {
    boolean definesThis =
        arguments.stream().anyMatch(arg -> arg.getName().equals("this") && arg.isPositional());
    if (!definesThis) {
      processingEnvironment
          .getMessager()
          .printMessage(
              Diagnostic.Kind.ERROR,
              "The execute method does not take `this` argument. At least one positional argument must be named `_this`.",
              element);
    }
    return definesThis;
  }

  /** @return the package name this method was declared in. */
  public String getPackageName() {
    return packageName;
  }

  /** @return the original class name of this method. */
  public String getOriginalClassName() {
    return originalClassName;
  }

  /** @return the simple class name of generated method wrapper. */
  public String getClassName() {
    return className;
  }

  /** @return full name (with package) of the generated wrapper. */
  public String getQualifiedName() {
    return qualifiedName;
  }

  /** @return the language-level name of this method. */
  public String getDeclaredName() {
    return annotation.type() + "." + annotation.name();
  }

  /** @return get the description of this method. */
  public String getDescription() {
    return annotation.description();
  }

  /** @return the arguments this method declares. */
  public List<ArgumentDefinition> getArguments() {
    return arguments;
  }

  /** @return the additional imports this method definition requires. */
  public Set<String> getImports() {
    return imports;
  }

  /** @return whether this method modifies the monadic state. */
  public boolean modifiesState() {
    return modifiesState;
  }

  /** @return whether this method requires caller info to work properly. */
  public boolean needsCallerInfo() {
    return needsCallerInfo;
  }

  public String getConstructorExpression() {
    return constructorExpression;
  }

  /** A domain specific representation of a method argument. */
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

    /**
     * Creates a new instance of this class.
     *
     * @param element the element representing this argument.
     * @param position the position (0-indexed) of this argument in the arguments list.
     */
    public ArgumentDefinition(VariableElement element, int position) {
      type = element.asType();
      String[] typeNameSegments = type.toString().split("\\.");
      typeName = typeNameSegments[typeNameSegments.length - 1];
      String originalName = element.getSimpleName().toString();
      name = originalName.equals("_this") ? "this" : originalName;
      isState = element.getAnnotation(MonadicState.class) != null && type.toString().equals(OBJECT);
      isFrame = type.toString().equals(VIRTUAL_FRAME);
      isCallerInfo = type.toString().equals(CALLER_INFO);
      this.position = position;
    }

    /** @return whether this argument should be passed the monadic state. */
    public boolean isState() {
      return isState;
    }

    /** @return whether this argument should be passed the execution frame. */
    public boolean isFrame() {
      return isFrame;
    }

    /** @return whether this argument should be passed the caller info. */
    public boolean isCallerInfo() {
      return isCallerInfo;
    }

    /** @return whether this argument should be passed the next positional function argument. */
    public boolean isPositional() {
      return !isFrame() && !isState() && !isCallerInfo();
    }

    /** @return the position of this argument in the positional arguments list. */
    public int getPosition() {
      return position;
    }

    /** @return any import this argument requires. */
    public Optional<String> getImport() {
      if (type.getKind() == TypeKind.DECLARED) {
        if (!type.toString().equals(OBJECT)) {
          return Optional.of(type.toString());
        }
      }
      return Optional.empty();
    }

    /** @return whether this argument needs to be type-casted on read. */
    public boolean requiresCast() {
      return !type.toString().equals(OBJECT);
    }

    /** @return the name of the type of this argument. */
    public String getTypeName() {
      return typeName;
    }

    /** @return the type of this argument. */
    public TypeMirror getType() {
      return type;
    }

    /** @return the name of this argument. */
    public String getName() {
      return name;
    }

    /** @return whether this argument is expected to be passed suspended. */
    public boolean isSuspended() {
      return type.toString().equals(THUNK);
    }
  }
}
