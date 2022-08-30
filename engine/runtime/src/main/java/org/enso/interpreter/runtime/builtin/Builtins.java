package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.compiler.Passes;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.exception.CompilerError;
import org.enso.compiler.phase.BuiltinsIrBuilder;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.TypeProcessor;
import org.enso.interpreter.dsl.model.MethodDefinition;
import org.enso.interpreter.node.expression.builtin.Boolean;
import org.enso.interpreter.node.expression.builtin.*;
import org.enso.interpreter.node.expression.builtin.debug.Debug;
import org.enso.interpreter.node.expression.builtin.error.CaughtPanic;
import org.enso.interpreter.node.expression.builtin.error.Warning;
import org.enso.interpreter.node.expression.builtin.io.File;
import org.enso.interpreter.node.expression.builtin.meta.ProjectDescription;
import org.enso.interpreter.node.expression.builtin.mutable.Array;
import org.enso.interpreter.node.expression.builtin.mutable.Ref;
import org.enso.interpreter.node.expression.builtin.ordering.Ordering;
import org.enso.interpreter.node.expression.builtin.resource.ManagedResource;
import org.enso.interpreter.node.expression.builtin.text.Text;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.type.TypesFromProxy;
import org.enso.pkg.QualifiedName;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  public static final String PACKAGE_NAME = "Builtins";
  public static final String NAMESPACE = "Standard";
  public static final String MODULE_NAME = NAMESPACE + "." + PACKAGE_NAME + ".Main";

  /** Container for method names needed outside this class. */
  public static class MethodNames {
    public static class Debug {
      public static final String EVAL = "eval";
    }
  }

  private final Map<String, Map<String, Class<BuiltinRootNode>>> builtinMethodNodes;
  private final Map<Class<? extends Builtin>, Builtin> builtins;
  private final Map<String, Builtin> builtinsByName;

  private final Error error;
  private final Module module;
  private final ModuleScope scope;
  private final Number number;
  private final Boolean bool;
  private final Ordering ordering;
  private final System system;
  private final Special special;

  // Builtin types
  private final Builtin any;
  private final Builtin nothing;
  private final Builtin function;
  private final Builtin polyglot;
  private final Builtin text;
  private final Builtin array;
  private final Builtin dataflowError;
  private final Builtin ref;
  private final Builtin managedResource;
  private final Builtin debug;
  private final ProjectDescription projectDescription;
  private final Builtin file;
  private final Builtin date;
  private final Builtin dateTime;
  private final Builtin timeOfDay;
  private final Builtin timeZone;
  private final Builtin warning;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param context the current {@link Context} instance
   */
  public Builtins(Context context) {
    Language language = context.getLanguage();
    module = Module.empty(QualifiedName.fromString(MODULE_NAME), null, null);
    scope = module.compileScope(context);

    builtins = readBuiltinTypesMetadata(language, scope);
    builtinsByName =
        builtins.values().stream().collect(Collectors.toMap(v -> v.getType().getName(), v -> v));
    builtinMethodNodes = readBuiltinMethodsMetadata(scope);
    registerBuiltinMethods(scope, language);

    error = new Error(this, context);
    ordering = getBuiltinType(Ordering.class);
    system = new System(this);
    number = new Number(this);
    bool = this.getBuiltinType(Boolean.class);

    any = builtins.get(Any.class);
    nothing = builtins.get(Nothing.class);
    function = builtins.get(org.enso.interpreter.node.expression.builtin.function.Function.class);
    polyglot = builtins.get(Polyglot.class);
    text = builtins.get(Text.class);
    array = builtins.get(Array.class);
    dataflowError = builtins.get(org.enso.interpreter.node.expression.builtin.Error.class);
    ref = builtins.get(Ref.class);
    managedResource = builtins.get(ManagedResource.class);
    debug = builtins.get(Debug.class);
    projectDescription = getBuiltinType(ProjectDescription.class);
    file = builtins.get(File.class);
    date = builtins.get(org.enso.interpreter.node.expression.builtin.date.Date.class);
    dateTime = builtins.get(org.enso.interpreter.node.expression.builtin.date.DateTime.class);
    timeOfDay = builtins.get(org.enso.interpreter.node.expression.builtin.date.TimeOfDay.class);
    timeZone = builtins.get(org.enso.interpreter.node.expression.builtin.date.TimeZone.class);
    special = new Special(language);
    warning = builtins.get(Warning.class);
  }

  /**
   * Registers builtin methods with their corresponding Atom Constructor's owners. That way
   * "special" builtin types have builtin methods in the scope without requiring everyone to always
   * import full stdlib
   *
   * @param scope Builtins scope
   * @param language The language the resulting function nodes should be associated with
   */
  private void registerBuiltinMethods(ModuleScope scope, Language language) {
    for (Builtin builtin : builtins.values()) {
      var type = builtin.getType();
      String tpeName = type.getName();
      Map<String, Class<BuiltinRootNode>> methods = builtinMethodNodes.get(tpeName);
      if (methods != null) {
        methods.forEach(
            (methodName, clazz) -> {
              Optional<Function> fun;
              try {
                Method meth = clazz.getMethod("makeFunction", Language.class);
                fun = Optional.ofNullable((Function) meth.invoke(null, language));
              } catch (Exception e) {
                e.printStackTrace();
                fun = Optional.empty();
              }
              fun.ifPresent(f -> scope.registerMethod(type, methodName, f));
            });
      }
    }
  }

  /** @return {@code true} if the IR has been initialized, otherwise {@code false} */
  public boolean isIrInitialized() {
    return this.module.getIr() != null;
  }

  /** Initialize the source file for the builtins module. */
  @CompilerDirectives.TruffleBoundary
  public void initializeBuiltinsSource() {
    module.setLiteralSource("");
  }

  /**
   * Initialize the IR for the builtins module from the builtins source file.
   *
   * @param freshNameSupply the compiler's fresh name supply
   * @param passes the passes manager for the compiler
   */
  @CompilerDirectives.TruffleBoundary
  public void initializeBuiltinsIr(FreshNameSupply freshNameSupply, Passes passes) {
    try {
      if (module.getSource() == null) {
        initializeBuiltinsSource();
      }
      BuiltinsIrBuilder.build(module, freshNameSupply, passes);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Returns a list of supported builtins.
   *
   * <p>Builtin types are marked via @BuiltinType annotation. THe metdata file represents a single
   * builtin type per row. The format of the row is as follows: <Enso name of the builtin
   * type>:<Name of the class representing it>:[<field1>,<field2>,...] where the last column gives a
   * list of optional type's fields.
   *
   * @param scope Builtins scope
   */
  private Map<Class<? extends Builtin>, Builtin> readBuiltinTypesMetadata(
      Language language, ModuleScope scope) {
    ClassLoader classLoader = getClass().getClassLoader();
    List<String> lines;
    try (InputStream resource = classLoader.getResourceAsStream(TypeProcessor.META_PATH)) {
      lines =
          new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
              .lines()
              .collect(Collectors.toList());
    } catch (Exception ioe) {
      lines = new ArrayList<>();
      ioe.printStackTrace();
    }

    Map<Class<? extends Builtin>, Builtin> builtins =
        lines.stream()
            .map(
                line -> {
                  String[] builtinMeta = line.split(":");
                  if (builtinMeta.length < 2 || builtinMeta.length > 3) {
                    java.lang.System.out.println(Arrays.toString(builtinMeta));
                    throw new CompilerError("Invalid builtin metadata in: " + line);
                  }
                  try {
                    @SuppressWarnings("unchecked")
                    var cls = (Class<? extends Builtin>) Class.forName(builtinMeta[1]);
                    return cls.getConstructor().newInstance();
                  } catch (NoSuchMethodException
                      | InstantiationException
                      | IllegalAccessException
                      | InvocationTargetException
                      | ClassNotFoundException e) {
                    e.printStackTrace();
                    throw new CompilerError("Invalid builtin type entry: " + builtinMeta[1]);
                  }
                })
            .collect(Collectors.toMap(Builtin::getClass, b -> b));
    builtins.values().forEach(b -> b.initialize(language, scope, builtins));
    return builtins;
  }

  /**
   * Returns a Map of builtin method nodes.
   *
   * <p>Builtin types are marked via @BuiltinMethod annotation. THe metdata file represents a single
   * builtin method per row. The format of the row is as follows: <Fully qualified name of the
   * builtin method>:<Class name of the builtin method representing it>
   *
   * @param scope Builtins scope
   * @return A map of builtin method nodes per builtin type name
   */
  private Map<String, Map<String, Class<BuiltinRootNode>>> readBuiltinMethodsMetadata(
      ModuleScope scope) {
    ClassLoader classLoader = getClass().getClassLoader();
    List<String> lines;
    try (InputStream resource = classLoader.getResourceAsStream(MethodDefinition.META_PATH)) {
      lines =
          new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
              .lines()
              .collect(Collectors.toList());
    } catch (Exception ioe) {
      lines = new ArrayList<>();
      ioe.printStackTrace();
    }
    Map<String, Map<String, Class<BuiltinRootNode>>> methodNodes = new HashMap<>();

    lines.forEach(
        line -> {
          String[] builtinMeta = line.split(":");
          if (builtinMeta.length != 2) {
            throw new CompilerError("Invalid builtin metadata in: " + line);
          }
          String[] builtinName = builtinMeta[0].split("\\.");
          if (builtinName.length != 2) {
            throw new CompilerError("Invalid builtin metadata in : " + line);
          }
          try {
            @SuppressWarnings("unchecked")
            Class<BuiltinRootNode> clazz = (Class<BuiltinRootNode>) Class.forName(builtinMeta[1]);
            String builtinMethodOwner = builtinName[0];
            String builtinMethodName = builtinName[1];
            scope
                .getLocalConstructor(builtinMethodOwner)
                .ifPresentOrElse(
                    constr -> {
                      Map<String, Class<BuiltinRootNode>> atomNodes =
                          methodNodes.get(builtinMethodOwner);
                      if (atomNodes == null) {
                        atomNodes = new HashMap<>();
                        // TODO: move away from String Map once Builtins are gone
                        methodNodes.put(constr.getName(), atomNodes);
                      }
                      atomNodes.put(builtinMethodName, clazz);
                    },
                    () -> {
                      Map<String, Class<BuiltinRootNode>> atomNodes =
                          methodNodes.get(builtinMethodOwner);
                      if (atomNodes == null) {
                        atomNodes = new HashMap<>();
                        // TODO: move away from String Map once Builtins are gone
                        methodNodes.put(builtinMethodOwner, atomNodes);
                      }
                      atomNodes.put(builtinMethodName, clazz);
                    });
          } catch (ClassNotFoundException e) {
            e.printStackTrace();
          }
        });
    return methodNodes;
  }

  /**
   * Returns a builtin method for the provided Atom Constructor and the name, if it exists.
   *
   * @param type Atom Constructor owner of the function
   * @param methodName Name of the method
   * @param language The language the resulting function nodes should be associated with
   * @return A non-empty function under the given name, if it exists. An empty value if no such
   *     builtin method was ever registerd
   */
  public Optional<Function> getBuiltinFunction(String type, String methodName, Language language) {
    // TODO: move away from String mapping once Builtins is gone
    Map<String, Class<BuiltinRootNode>> atomNodes = builtinMethodNodes.get(type);
    if (atomNodes == null) return Optional.empty();
    Class<BuiltinRootNode> clazz = atomNodes.get(methodName);
    if (clazz == null) return Optional.empty();
    try {
      Method meth = clazz.getMethod("makeFunction", Language.class);
      return Optional.ofNullable((Function) meth.invoke(null, language));
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public Optional<Function> getBuiltinFunction(Type type, String methodName, Language language) {
    return getBuiltinFunction(type.getName(), methodName, language);
  }

  public <T extends Builtin> T getBuiltinType(Class<T> clazz) {
    @SuppressWarnings("unchecked")
    T t = (T) builtins.get(clazz);
    return t;
  }

  public Builtin getBuiltinType(String name) {
    return builtinsByName.get(name);
  }

  /**
   * Returns the {@code Nothing} atom constructor.
   *
   * @return the {@code Nothing} atom constructor
   */
  public Type nothing() {
    return nothing.getType();
  }

  /**
   * Returns the {@code Text} part of builtins.
   *
   * @return the {@code Text} part of builtins.
   */
  public Type text() {
    return text.getType();
  }

  /**
   * Returns the {@code Function} atom constructor.
   *
   * @return the {@code Function} atom constructor
   */
  public Type function() {
    return function.getType();
  }

  /**
   * Returns the number-related entities.
   *
   * @return the number-related part of builtins.
   */
  public Number number() {
    return number;
  }

  /** @return the container for boolean constructors. */
  public Boolean bool() {
    return bool;
  }

  /** @return the ManagedResource constructor. */
  public Type managedResource() {
    return managedResource.getType();
  }

  /** @return the builtin Error types container. */
  public Error error() {
    return error;
  }

  /**
   * Returns the {@code Any} atom constructor.
   *
   * @return the {@code Any} atom constructor
   */
  public Type any() {
    return any.getType();
  }

  /**
   * Returns the {@code Warning} atom constructor.
   *
   * @return the {@code Warning} atom constructor
   */
  public Type warning() {
    return warning.getType();
  }

  /**
   * Returns the {@code File} atom constructor.
   *
   * @return the {@code File} atom constructor
   */
  public Type file() {
    return file.getType();
  }

  /**
   * Returns the {@code Date} atom constructor.
   *
   * @return the {@code Date} atom constructor
   */
  public Type date() {
    return date.getType();
  }

  /**
   * Returns the {@code DateTime} atom constructor.
   *
   * @return the {@code DateTime} atom constructor
   */
  public Type dateTime() {
    return dateTime.getType();
  }

  /**
   * Returns the {@code TimeOfDay} atom constructor.
   *
   * @return the {@code TimeOfDay} atom constructor
   */
  public Type timeOfDay() {
    return timeOfDay.getType();
  }

  /**
   * Returns the {@code TimeZone} atom constructor.
   *
   * @return the {@code TimeZone} atom constructor
   */
  public Type timeZone() {
    return timeZone.getType();
  }

  /**
   * Returns the {@code Debug} atom constructor. TODO: this is redundant, figure out a way to avoid
   * createing spurious Debug builtin type
   *
   * @return the {@code Debug} atom constructor
   */
  public Type debug() {
    return debug.getType();
  }

  /** @return the {@code Project_Description} atom constructor */
  public ProjectDescription getProjectDescription() {
    return projectDescription;
  }

  /** @return the {@code System} atom constructor. */
  public System system() {
    return system;
  }

  /** @return the Array constructor. */
  public Type array() {
    return array.getType();
  }

  /** @return the Ref constructor. */
  public Type ref() {
    return ref.getType();
  }

  /** @return the container for polyglot-related builtins. */
  public Type polyglot() {
    return polyglot.getType();
  }

  /** @return the {@code Caught_Panic} atom constructor */
  public CaughtPanic caughtPanic() {
    return this.error.caughtPanic();
  }

  /** @return the {@code Panic} atom constructor */
  public Type panic() {
    return this.error.panic();
  }

  /** @return the container for ordering-related builtins */
  public Ordering ordering() {
    return ordering;
  }

  /** @return the container for the dataflow error-related builtins */
  public Type dataflowError() {
    return dataflowError.getType();
  }

  public Special special() {
    return special;
  }

  /**
   * Returns the builtin module scope.
   *
   * @return the builtin module scope
   */
  public ModuleScope getScope() {
    return scope;
  }

  public Module getModule() {
    return module;
  }

  /**
   * Convert from type-system type names to types.
   *
   * @param typeName the fully qualified type name of a builtin
   * @return the associated {@link org.enso.interpreter.runtime.callable.atom.Atom} if it exists,
   *     and {@code null} otherwise
   */
  public Type fromTypeSystem(String typeName) {
    return TypesFromProxy.fromTypeSystem(this, typeName);
  }
}
