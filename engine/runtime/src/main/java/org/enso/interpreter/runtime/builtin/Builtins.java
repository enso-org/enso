package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

import org.enso.compiler.Passes;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.exception.CompilerError;
import org.enso.compiler.phase.BuiltinsIrBuilder;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.TypeProcessor;
import org.enso.interpreter.dsl.model.MethodDefinition;
import org.enso.interpreter.node.expression.builtin.*;
import org.enso.interpreter.node.expression.builtin.debug.Debug;
import org.enso.interpreter.node.expression.builtin.error.Warning;
import org.enso.interpreter.node.expression.builtin.io.File;
import org.enso.interpreter.node.expression.builtin.meta.ProjectDescription;
import org.enso.interpreter.node.expression.builtin.mutable.Array;
import org.enso.interpreter.node.expression.builtin.mutable.Ref;
import org.enso.interpreter.node.expression.builtin.resource.ManagedResource;
import org.enso.interpreter.node.expression.builtin.text.Text;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.type.TypesFromProxy;
import org.enso.pkg.QualifiedName;

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
  private final Map<String, AtomConstructor> builtins;

  private final Error error;
  private final Module module;
  private final ModuleScope scope;
  public final Number number;
  private final Bool bool;
  private final Ordering ordering;
  private final System system;
  private final Special special;

  // Builtin types
  private final BuiltinAtomConstructor any;
  private final BuiltinAtomConstructor nothing;
  private final BuiltinAtomConstructor function;
  private final BuiltinAtomConstructor polyglot;
  private final BuiltinAtomConstructor text;
  private final BuiltinAtomConstructor array;
  private final BuiltinAtomConstructor dataflowError;
  private final BuiltinAtomConstructor ref;
  private final BuiltinAtomConstructor managedResource;
  private final BuiltinAtomConstructor debug;
  private final BuiltinAtomConstructor projectDescription;
  private final BuiltinAtomConstructor file;
  private final BuiltinAtomConstructor date;
  private final BuiltinAtomConstructor warning;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param context the current {@link Context} instance
   */
  public Builtins(Context context) {
    Language language = context.getLanguage();
    module = Module.empty(QualifiedName.fromString(MODULE_NAME), null);
    scope = module.compileScope(context);

    builtins = new HashMap<>();
    List<AtomConstructor> builtinTypes = readBuiltinTypesMetadata(scope);
    builtinMethodNodes = readBuiltinMethodsMetadata(scope);
    registerBuiltinMethods(builtinTypes, scope, language);

    error = new Error(this, context);
    ordering = new Ordering(this);
    system = new System(this);
    number = new Number(this);
    bool = new Bool(this);

    any = new BuiltinAtomConstructor(this, Any.class);
    nothing = new BuiltinAtomConstructor(this, Nothing.class);
    function =
        new BuiltinAtomConstructor(
            this, org.enso.interpreter.node.expression.builtin.function.Function.class);
    polyglot = new BuiltinAtomConstructor(this, Polyglot.class);
    text = new BuiltinAtomConstructor(this, Text.class);
    array = new BuiltinAtomConstructor(this, Array.class);
    dataflowError =
        new BuiltinAtomConstructor(this, org.enso.interpreter.node.expression.builtin.Error.class);
    ref = new BuiltinAtomConstructor(this, Ref.class);
    managedResource = new BuiltinAtomConstructor(this, ManagedResource.class);
    debug = new BuiltinAtomConstructor(this, Debug.class);
    projectDescription = new BuiltinAtomConstructor(this, ProjectDescription.class);
    file = new BuiltinAtomConstructor(this, File.class);
    date =
        new BuiltinAtomConstructor(
            this, org.enso.interpreter.node.expression.builtin.date.Date.class);
    special = new Special(language);
    warning = new BuiltinAtomConstructor(this, Warning.class);
  }

  /**
   * Registers builtin methods with their corresponding Atom Constructor's owners. That way
   * "special" builtin types have builtin methods in the scope without requiring everyone to always
   * import full stdlib
   *
   * @param builtins List of Builtin Types
   * @param scope Builtins scope
   * @param language The language the resulting function nodes should be associated with
   */
  private void registerBuiltinMethods(
      List<AtomConstructor> builtins, ModuleScope scope, Language language) {
    for (AtomConstructor atom : builtins) {
      String tpeName = atom.getName();
      this.builtins.put(tpeName, atom);
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
              fun.ifPresent(f -> scope.registerMethod(atom, methodName, f));
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
  private List<AtomConstructor> readBuiltinTypesMetadata(ModuleScope scope) {
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

    return lines.stream()
        .map(
            line -> {
              String[] builtinMeta = line.split(":");
              if (builtinMeta.length < 2 || builtinMeta.length > 4) {
                throw new CompilerError("Invalid builtin metadata in: " + line);
              }

              AtomConstructor builtin;
              builtin = new AtomConstructor(builtinMeta[0], scope, true);

              if (builtinMeta.length < 3 || builtinMeta[2].isEmpty()) {
                builtin = builtin.initializeFields();
              } else {
                // there are some type params
                String[] paramNames = builtinMeta[2].split(",");
                ArgumentDefinition[] args = new ArgumentDefinition[paramNames.length];
                for (int i = 0; i < paramNames.length; i++) {
                  args[i] =
                      new ArgumentDefinition(
                          i, paramNames[i], ArgumentDefinition.ExecutionMode.EXECUTE);
                }
                builtin = builtin.initializeFields(args);
              }
              return builtin;
            })
        .filter(b -> b != null)
        .collect(Collectors.toList());
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
   * @param atom Atom Constructor owner of the function
   * @param methodName Name of the method
   * @param language The language the resulting function nodes should be associated with
   * @return A non-empty function under the given name, if it exists. An empty value if no such
   *     builtin method was ever registerd
   */
  public Optional<Function> getBuiltinFunction(
      AtomConstructor atom, String methodName, Language language) {
    // TODO: move away from String mapping once Builtins is gone
    Map<String, Class<BuiltinRootNode>> atomNodes = builtinMethodNodes.get(atom.getName());
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

  public AtomConstructor getBuiltinType(Class<? extends Builtin> clazz) {
    String snakeCaseName = clazz.getSimpleName().replaceAll("([^_A-Z])([A-Z])", "$1_$2");
    return getBuiltinType(snakeCaseName);
  }

  public AtomConstructor getBuiltinType(String name) {
    return builtins.get(name);
  }

  /**
   * Returns the {@code Nothing} atom constructor.
   *
   * @return the {@code Nothing} atom constructor
   */
  public AtomConstructor nothing() {
    return nothing.constructor();
  }

  /**
   * Returns the {@code Text} part of builtins.
   *
   * @return the {@code Text} part of builtins.
   */
  public AtomConstructor text() {
    return text.constructor();
  }

  /**
   * Returns the {@code Function} atom constructor.
   *
   * @return the {@code Function} atom constructor
   */
  public AtomConstructor function() {
    return function.constructor();
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
  public Bool bool() {
    return bool;
  }

  /** @return the ManagedResource constructor. */
  public AtomConstructor managedResource() {
    return managedResource.constructor();
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
  public AtomConstructor any() {
    return any.constructor();
  }

  /**
   * Returns the {@code Warning} atom constructor.
   *
   * @return the {@code Warning} atom constructor
   */
  public AtomConstructor warning() {
    return warning.constructor();
  }

  /**
   * Returns the {@code File} atom constructor.
   *
   * @return the {@code File} atom constructor
   */
  public AtomConstructor file() {
    return file.constructor();
  }

  /**
   * Returns the {@code Date} atom constructor.
   *
   * @return the {@code Date} atom constructor
   */
  public AtomConstructor date() {
    return date.constructor();
  }

  /**
   * Returns the {@code Debug} atom constructor. TODO: this is redundant, figure out a way to avoid
   * createing spurious Debug builtin type
   *
   * @return the {@code Debug} atom constructor
   */
  public AtomConstructor debug() {
    return debug.constructor();
  }

  /** @return the {@code Project_Description} atom constructor */
  public AtomConstructor getProjectDescription() {
    return projectDescription.constructor();
  }

  /** @return the {@code System} atom constructor. */
  public System system() {
    return system;
  }

  /** @return the Array constructor. */
  public AtomConstructor array() {
    return array.constructor();
  }

  /** @return the Ref constructor. */
  public AtomConstructor ref() {
    return ref.constructor();
  }

  /** @return the container for polyglot-related builtins. */
  public AtomConstructor polyglot() {
    return polyglot.constructor();
  }

  /** @return the {@code Caught_Panic} atom constructor */
  public AtomConstructor caughtPanic() {
    return this.error.caughtPanic();
  }

  /** @return the {@code Panic} atom constructor */
  public AtomConstructor panic() {
    return this.error.panic();
  }

  /** @return the container for ordering-related builtins */
  public Ordering ordering() {
    return ordering;
  }

  /** @return the container for the dataflow error-related builtins */
  public AtomConstructor dataflowError() {
    return dataflowError.constructor();
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
   * Convert from type-system type names to atoms.
   *
   * @param typeName the fully qualified type name of a builtin
   * @return the associated {@link org.enso.interpreter.runtime.callable.atom.Atom} if it exists,
   *     and {@code null} otherwise
   */
  public Atom fromTypeSystem(String typeName) {
    return TypesFromProxy.fromTypeSystem(this, typeName);
  }
}
