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
import org.enso.interpreter.node.expression.builtin.Boolean;
import org.enso.interpreter.node.expression.builtin.bool.False;
import org.enso.interpreter.node.expression.builtin.bool.True;
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
import org.enso.interpreter.runtime.type.Constants;
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

  @CompilerDirectives.CompilationFinal private AtomConstructor debug;
  @CompilerDirectives.CompilationFinal private AtomConstructor projectDescription;

  private final Error error;
  private final Module module;
  private final ModuleScope scope;
  public final Number number;
  private final Ordering ordering;
  private final System system;
  private final Special special;

  // Builtin types
  @CompilerDirectives.CompilationFinal private AtomConstructor any;

  @CompilerDirectives.CompilationFinal private AtomConstructor nothing;

  @CompilerDirectives.CompilationFinal private AtomConstructor function;

  @CompilerDirectives.CompilationFinal private AtomConstructor polyglot;

  @CompilerDirectives.CompilationFinal private AtomConstructor text;

  @CompilerDirectives.CompilationFinal private AtomConstructor array;

  @CompilerDirectives.CompilationFinal private AtomConstructor bool;

  @CompilerDirectives.CompilationFinal private AtomConstructor trueConstructor;

  @CompilerDirectives.CompilationFinal private AtomConstructor falseConstructor;

  @CompilerDirectives.CompilationFinal private AtomConstructor dataflowError;

  @CompilerDirectives.CompilationFinal private AtomConstructor ref;

  @CompilerDirectives.CompilationFinal private AtomConstructor managedResource;

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

    error = new Error(this);
    ordering = new Ordering(this);
    system = new System(this);
    number = new Number(this);
    special = new Special(language);
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
              if (builtinMeta.length != 4) {
                throw new CompilerError(
                    "Invalid builtin metadata in: " + line + " " + builtinMeta.length);
              }

              AtomConstructor builtin;
              builtin = new AtomConstructor(builtinMeta[0], scope, true);

              if (builtinMeta[3].isEmpty()) {
                builtin = builtin.initializeFields();
              } else {
                // there are some type params
                String[] paramNames = builtinMeta[3].split(",");
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
    if (nothing == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      nothing = getBuiltinType(Nothing.class);
    }
    return nothing;
  }

  /**
   * Returns the {@code Text} part of builtins.
   *
   * @return the {@code Text} part of builtins.
   */
  public AtomConstructor text() {
    if (text == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      text = getBuiltinType(Text.class);
    }
    return text;
  }

  /**
   * Returns the {@code Function} atom constructor.
   *
   * @return the {@code Function} atom constructor
   */
  public AtomConstructor function() {
    if (function == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      function =
          getBuiltinType(org.enso.interpreter.node.expression.builtin.function.Function.class);
    }
    return function;
  }

  /**
   * Returns the number-related entities.
   *
   * @return the number-related part of builtins.
   */
  public Number number() {
    return number;
  }

  /** @return the Boolean constructor. */
  public AtomConstructor bool() {
    if (bool == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      bool = getBuiltinType(Boolean.class);
    }
    return bool;
  }

  /** @return the True constructor. */
  public AtomConstructor trueAtom() {
    if (trueConstructor == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      trueConstructor = getBuiltinType(True.class);
    }
    return trueConstructor;
  }

  /** @return the False constructor. */
  public AtomConstructor falseAtom() {
    if (falseConstructor == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      falseConstructor = getBuiltinType(False.class);
    }
    return falseConstructor;
  }

  /** @return the ManagedResource constructor. */
  public AtomConstructor managedResource() {
    if (managedResource == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      managedResource = getBuiltinType(ManagedResource.class);
    }
    return managedResource;
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
    if (any == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      any = getBuiltinType(Any.class);
    }
    return any;
  }

  /**
   * Returns the {@code Debug} atom constructor. TODO: this is redundant, figure out a way to avoid
   * createing spurious Debug builtin type
   *
   * @return the {@code Debug} atom constructor
   */
  public AtomConstructor debug() {
    if (debug == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      debug = getBuiltinType(Debug.class);
    }
    return debug;
  }

  /** @return the {@code Project_Description} atom constructor */
  public AtomConstructor getProjectDescription() {
    if (projectDescription == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      projectDescription = getBuiltinType(ProjectDescription.class);
    }
    return projectDescription;
  }

  /** @return the {@code System} atom constructor. */
  public System system() {
    return system;
  }

  /** @return the Array constructor. */
  public AtomConstructor array() {
    if (array == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      array = getBuiltinType(Array.class);
    }
    return array;
  }

  /** @return the Ref constructor. */
  public AtomConstructor ref() {
    if (ref == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      ref = getBuiltinType(Ref.class);
    }
    return ref;
  }

  /** @return the container for polyglot-related builtins. */
  public AtomConstructor polyglot() {
    if (polyglot == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      polyglot = getBuiltinType(Polyglot.class);
    }
    return polyglot;
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
    if (dataflowError == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      dataflowError = getBuiltinType(org.enso.interpreter.node.expression.builtin.Error.class);
    }
    return dataflowError;
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
