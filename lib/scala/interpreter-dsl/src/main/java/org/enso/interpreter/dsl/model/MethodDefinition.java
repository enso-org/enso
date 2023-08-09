package org.enso.interpreter.dsl.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic;
import javax.tools.Diagnostic.Kind;

import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;

/** A domain-specific representation of a builtin method. */
public class MethodDefinition {
  public static final String NODE_PKG = "org.enso.interpreter.node.expression.builtin";
  public static final String META_PATH =
      "META-INF" + "/" + NODE_PKG.replace('.', '/') + "/BuiltinMethods.metadata";

  private final String packageName;
  private final String originalClassName;
  private final String className;
  private final String qualifiedName;
  private final BuiltinMethod annotation;
  private final TypeElement element;
  private final ExecutableElement executeMethod;
  private final List<ArgumentDefinition> arguments;
  private final Set<String> imports;
  private final boolean needsCallerInfo;
  private final boolean needsFrame;
  private final Object constructorExpression;

  /**
   * Creates a new instance of this class.
   *
   * @param packageName the name of the package this method is declared in.
   * @param element the element (class) declaring this method.
   * @param execute the element (method) containing the logic.
   * @param needsFrame optionally specify if we need own frame, if {@code null} the value is derived
   *     from presence/absence of {@code VirtualFrame} argument
   */
  public MethodDefinition(
      String packageName, TypeElement element, ExecutableElement execute, Boolean needsFrame) {
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
    this.needsFrame =
        needsFrame != null ? needsFrame : arguments.stream().anyMatch(ArgumentDefinition::isFrame);
    this.constructorExpression = initConstructor(element);
  }

  public boolean hasAliases() {
    return !annotation.aliases().isEmpty();
  }

  public String[] aliases() {
    if (annotation.aliases().isEmpty()) {
      return new String[0];
    } else {
      String[] methodNames = annotation.aliases().split(",");
      for (int i = 0; i < methodNames.length; i++) {
        methodNames[i] = annotation.type() + "." + methodNames[i];
      }
      return methodNames;
    }
  }

  private Object initConstructor(TypeElement element) {
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
      boolean isClassAbstract = element.getModifiers().contains(Modifier.ABSTRACT);
      if (isClassAbstract) {
        return new RuntimeException(
            "Class "
                + element.getSimpleName()
                + " is abstract, and has no static `build()` method.");
      } else {
        return "new " + originalClassName + "()";
      }
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
    boolean noSelfDeclared =
        params.stream()
            .filter(e -> e.getSimpleName().toString().equals("self"))
            .findFirst()
            .isEmpty();
    boolean needsSelf = noSelfDeclared;
    int position = 0;
    for (VariableElement param : params) {
      ArgumentDefinition def = new ArgumentDefinitionFromParameter(param, position);
      if (needsSelf && def.isPositional()) {
        args.add(new SelfArgumentDefinition());
        position++;
        def.incPosition();
        needsSelf = false;
      }
      args.add(def);
      if (def.isPositional()) {
        position++;
      }
    }
    if (needsSelf) {
      args.add(new SelfArgumentDefinition());
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
    if (this.constructorExpression instanceof Exception ex) {
      processingEnvironment.getMessager().printMessage(Kind.ERROR, ex.getMessage(), element);
      return false;
    }

    if (!executeMethod.getThrownTypes().isEmpty()) {
      processingEnvironment
          .getMessager()
          .printMessage(
              Kind.ERROR,
              "Builtin methods cannot throw exceptions",
              executeMethod);
      return false;
    }

    boolean argsValid = arguments.stream().allMatch(arg -> arg.validate(processingEnvironment));

    return argsValid;
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

  /** @return the language-level owner type of this method. */
  public String getType() {
    return annotation.type();
  }

  /** @return get the description of this method. */
  public String getDescription() {
    return annotation.description().replace("\n", "\\n");
  }

  /** @return the arguments this method declares. */
  public List<ArgumentDefinition> getArguments() {
    return arguments;
  }

  /** @return the additional imports this method definition requires. */
  public Set<String> getImports() {
    return imports;
  }

  /** @return whether this method requires caller info to work properly. */
  public boolean needsCallerInfo() {
    return needsCallerInfo;
  }

  /** @return whether this method requires virtual frame. */
  public boolean needsFrame() {
    return needsFrame;
  }

  public String getConstructorExpression() {
    return (String) constructorExpression;
  }

  public boolean isStatic() {
    return arguments.stream()
        .filter(arg -> arg.isSelf())
        .findFirst()
        .map(arg -> arg.isSyntheticSelf())
        .orElseGet(() -> false);
  }

  public boolean isAutoRegister() {
    return annotation.autoRegister();
  }

  public interface ArgumentDefinition {

    boolean validate(ProcessingEnvironment processingEnvironment);

    /** @return whether this argument should be passed the monadic state. */
    boolean isState();

    /** @return whether this argument should be passed the execution frame. */
    boolean isFrame();

    /** @return whether this argument should be passed the caller info. */
    boolean isCallerInfo();

    /** @return whether this argument should be passed the next positional function argument. */
    boolean isPositional();

    /** @return the position of this argument in the positional arguments list. */
    int getPosition();

    /** @return any import this argument requires. */
    Optional<String> getImport();

    /** @return whether this argument needs to be type-casted on read. */
    boolean requiresCast();

    boolean isArray();

    /** @return the name of the type of this argument. */
    String getTypeName();

    /** @return the name of this argument. */
    String getName();

    /** @return whether this argument is expected to be passed suspended. */
    boolean isSuspended();

    /** @return whether this argument accepts a dataflow error. */
    boolean acceptsError();

    boolean acceptsWarning();

    boolean isSelf();

    boolean isSyntheticSelf();

    boolean shouldCheckErrors();

    boolean shouldCheckWarnings();

    void incPosition();

    /**
     * @return whether the argument should be implicitly added by the processor and is not present
     *     in the signature
     */
    boolean isImplicit();
  }

  public class SelfArgumentDefinition implements ArgumentDefinition {

    @Override
    public boolean validate(ProcessingEnvironment processingEnvironment) {
      return true;
    }

    @Override
    public boolean isState() {
      return false;
    }

    @Override
    public boolean isFrame() {
      return false;
    }

    @Override
    public boolean isCallerInfo() {
      return false;
    }

    @Override
    public boolean isPositional() {
      return true;
    }

    @Override
    public int getPosition() {
      return 0;
    }

    @Override
    public Optional<String> getImport() {
      return Optional.empty();
    }

    @Override
    public boolean requiresCast() {
      return false;
    }

    @Override
    public boolean isArray() {
      return false;
    }

    @Override
    public String getTypeName() {
      return "Object";
    }

    @Override
    public String getName() {
      return "self";
    }

    @Override
    public boolean isSuspended() {
      return false;
    }

    @Override
    public boolean acceptsError() {
      return false;
    }

    @Override
    public boolean acceptsWarning() {
      return false;
    }

    @Override
    public boolean isSelf() {
      return true;
    }

    @Override
    public boolean isSyntheticSelf() {
      return true;
    }

    @Override
    public boolean shouldCheckErrors() {
      return false;
    }

    @Override
    public boolean shouldCheckWarnings() {
      return false;
    }

    @Override
    public void incPosition() {
      // noop
    }

    @Override
    public boolean isImplicit() {
      return true;
    }
  }

  /** A domain specific representation of a method argument. */
  public static class ArgumentDefinitionFromParameter implements ArgumentDefinition {
    private static final String VIRTUAL_FRAME = "com.oracle.truffle.api.frame.VirtualFrame";
    private static final String OBJECT = "java.lang.Object";
    private static final String THUNK = "org.enso.interpreter.runtime.callable.argument.Thunk";
    private static final String CALLER_INFO = "org.enso.interpreter.runtime.callable.CallerInfo";
    private static final String DATAFLOW_ERROR = "org.enso.interpreter.runtime.error.DataflowError";
    private static final String SELF = "self";

    private static final String STATE = "org.enso.interpreter.runtime.state.State";
    private final String typeName;
    private final TypeMirror type;
    private final String name;
    private final boolean isState;
    private final boolean isFrame;
    private final boolean isCallerInfo;
    private final boolean isSuspended;
    private final boolean acceptsError;
    private final boolean acceptsWarning;
    private int position;
    private final VariableElement element;

    /**
     * Creates a new instance of this class.
     *
     * @param element the element representing this argument.
     * @param position the position (0-indexed) of this argument in the arguments list.
     */
    public ArgumentDefinitionFromParameter(VariableElement element, int position) {
      this.element = element;
      type = element.asType();
      String[] typeNameSegments = type.toString().split("\\.");
      typeName = typeNameSegments[typeNameSegments.length - 1];
      name = element.getSimpleName().toString();
      isState = type.toString().equals(STATE);
      isSuspended = element.getAnnotation(Suspend.class) != null;
      acceptsError =
          (element.getAnnotation(AcceptsError.class) != null)
              || type.toString().equals(DATAFLOW_ERROR);
      acceptsWarning = element.getAnnotation(AcceptsWarning.class) != null;
      isFrame = type.toString().equals(VIRTUAL_FRAME);
      isCallerInfo = type.toString().equals(CALLER_INFO);
      this.position = position;
    }

    public boolean validate(ProcessingEnvironment processingEnvironment) {
      if (type.toString().equals(THUNK)) {
        processingEnvironment
            .getMessager()
            .printMessage(
                Diagnostic.Kind.ERROR,
                "Argument must not be typed as Thunk. Use @Suspend Object instead.",
                element);
        return false;
      }

      if (isSelf() && position != 0) {
        processingEnvironment
            .getMessager()
            .printMessage(
                Diagnostic.Kind.ERROR,
                "Argument `self` must be the first positional argument.",
                element);
        return false;
      }

      if (isPositional() && position == 0 && !isSelf()) {
        processingEnvironment
            .getMessager()
            .printMessage(
                Diagnostic.Kind.ERROR,
                "The first positional argument should be called `self`.",
                element);
        return false;
      }

      if (isState() && !type.toString().equals(STATE)) {
        processingEnvironment
            .getMessager()
            .printMessage(
                Diagnostic.Kind.ERROR,
                "The monadic state argument must be typed as " + STATE,
                element);
        return false;
      }

      return true;
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

    public void incPosition() {
      position = position + 1;
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

    public boolean isArray() {
      return type.toString().endsWith("[]");
    }

    /** @return the name of the type of this argument. */
    public String getTypeName() {
      return typeName;
    }

    /** @return the name of this argument. */
    public String getName() {
      return name;
    }

    /** @return whether this argument is expected to be passed suspended. */
    public boolean isSuspended() {
      return isSuspended;
    }

    /** @return whether thsi argument accepts a dataflow error. */
    public boolean acceptsError() {
      return acceptsError;
    }

    public boolean acceptsWarning() {
      return acceptsWarning;
    }

    public boolean isSelf() {
      return name.equals(SELF);
    }

    @Override
    public boolean isSyntheticSelf() {
      return false;
    }

    public boolean shouldCheckErrors() {
      return isPositional() && !isSelf() && !acceptsError() && !isSuspended();
    }

    public boolean shouldCheckWarnings() {
      return isPositional() && !isSelf() && !acceptsWarning() && !isSuspended();
    }

    public boolean isImplicit() {
      return false;
    }
  }
}
