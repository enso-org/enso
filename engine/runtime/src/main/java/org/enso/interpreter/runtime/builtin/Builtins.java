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
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointMethodGen;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalMethodGen;
import org.enso.interpreter.node.expression.builtin.error.CaughtPanic;
import org.enso.interpreter.node.expression.builtin.error.Panic;
import org.enso.interpreter.node.expression.builtin.function.ExplicitCallFunctionMethodGen;
import org.enso.interpreter.node.expression.builtin.mutable.Array;
import org.enso.interpreter.node.expression.builtin.mutable.Ref;
import org.enso.interpreter.node.expression.builtin.runtime.GCMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.NoInlineMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.NoInlineWithArgMethodGen;
import org.enso.interpreter.node.expression.builtin.state.GetStateMethodGen;
import org.enso.interpreter.node.expression.builtin.state.PutStateMethodGen;
import org.enso.interpreter.node.expression.builtin.state.RunStateMethodGen;
import org.enso.interpreter.node.expression.builtin.text.Text;
import org.enso.interpreter.node.expression.builtin.thread.WithInterruptHandlerMethodGen;
import org.enso.interpreter.node.expression.builtin.unsafe.SetAtomFieldMethodGen;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.type.Constants;
import org.enso.pkg.QualifiedName;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  public static final String PACKAGE_NAME = "Builtins";
  public static final String NAMESPACE = "Standard";
  public static final String SOURCE_NAME = PACKAGE_NAME + ".enso";
  public static final String MODULE_NAME = NAMESPACE + "." + PACKAGE_NAME + ".Main";

  /** Container for method names needed outside this class. */
  public static class MethodNames {
    public static class Debug {
      public static final String EVAL = "eval";
    }
  }

  private HashMap<String,Map<String, Class<BuiltinRootNode>>> builtinNodes;
  // TODO Consider dropping the map and just assigning to a single variable since builtin types
  //      should be unique
  private HashMap<String, AtomConstructor> builtinTypes;

  private final AtomConstructor debug;
  private final AtomConstructor projectDescription;

  private final Error error;
  private final Module module;
  private final ModuleScope scope;
  private final Number number;
  private final Ordering ordering;
  private final Resource resource;
  private final System system;
  private final Special special;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param context the current {@link Context} instance
   */
  public Builtins(Context context) {
    Language language = context.getLanguage();
    module = Module.empty(QualifiedName.fromString(MODULE_NAME), null);
    scope = module.compileScope(context);
    builtinNodes = new HashMap<>();
    builtinTypes = new HashMap<>();

    debug = new AtomConstructor("Debug", scope).initializeFields();
    Warning.initWarningMethods(language, scope);
    projectDescription =
        new AtomConstructor("Project_Description", scope)
            .initializeFields(
                new ArgumentDefinition(
                    0, "prim_root_file", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "prim_config", ArgumentDefinition.ExecutionMode.EXECUTE));
    error = new Error(this);
    number = new Number(language, scope);
    ordering = new Ordering(language, scope);
    resource = new Resource(language, scope);
    system = new System(language, scope);
    special = new Special(language);

    AtomConstructor runtime = new AtomConstructor("Runtime", scope).initializeFields();
    AtomConstructor state = new AtomConstructor("State", scope).initializeFields();

    AtomConstructor thread = new AtomConstructor("Thread", scope).initializeFields();

    AtomConstructor unsafe = new AtomConstructor("Unsafe", scope).initializeFields();

    scope.registerConstructor(state);
    scope.registerConstructor(debug);
    scope.registerConstructor(projectDescription);
    scope.registerConstructor(runtime);
    scope.registerConstructor(thread);

    scope.registerConstructor(unsafe);

    scope.registerMethod(runtime, "no_inline", NoInlineMethodGen.makeFunction(language));
    scope.registerMethod(
        runtime, "no_inline_with_arg", NoInlineWithArgMethodGen.makeFunction(language));
    scope.registerMethod(runtime, "gc", GCMethodGen.makeFunction(language));
    scope.registerMethod(
        runtime, "primitive_get_stack_trace", GetStackTraceMethodGen.makeFunction(language));

    scope.registerMethod(state, "get", GetStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "put", PutStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "run", RunStateMethodGen.makeFunction(language));

    scope.registerMethod(debug, MethodNames.Debug.EVAL, DebugEvalMethodGen.makeFunction(language));
    scope.registerMethod(debug, "breakpoint", DebugBreakpointMethodGen.makeFunction(language));

    scope.registerMethod(
        thread, "with_interrupt_handler", WithInterruptHandlerMethodGen.makeFunction(language));

    scope.registerMethod(unsafe, "set_atom_field", SetAtomFieldMethodGen.makeFunction(language));
    readBuiltinsMetadata(scope);
    assignMethodsToBuiltins(readBuiltinTypesMetadata(scope), scope, language);
  }

  public void assignMethodsToBuiltins(List<AtomConstructor> builtins, ModuleScope scope, Language language) {
    for (AtomConstructor atom: builtins) {
      String tpeName = atom.getName();
      builtinTypes.put(tpeName, atom);
      Map<String, Class<BuiltinRootNode>> methods = builtinNodes.get(tpeName);
      if (methods != null) {
        methods.forEach((methodName, clazz) -> {
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
    try {
      var builtinsModuleBytes =
          Objects.requireNonNull(
                  getClass().getClassLoader().getResourceAsStream(Builtins.SOURCE_NAME))
              .readAllBytes();
      String source = new String(builtinsModuleBytes, StandardCharsets.UTF_8);
      module.setLiteralSource(source);
    } catch (IOException e) {
      throw new CompilerError("Fatal, unable to read Builtins source file.");
    }
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

  private List<AtomConstructor> readBuiltinTypesMetadata(ModuleScope scope) {
    ClassLoader classLoader = getClass().getClassLoader();
    List<String> lines;
    try (InputStream resource = classLoader.getResourceAsStream(TypeProcessor.META_PATH)) {
      lines = new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8)).lines().collect(Collectors.toList());
    } catch (Exception ioe) {
      lines = new ArrayList<>();
      ioe.printStackTrace();
    }

    return lines.stream().map(line -> {
      String[] builtinMeta = line.split(":");
      if (builtinMeta.length < 2 || builtinMeta.length > 3) {
        throw new CompilerError("Invalid builtin metadata in: " + line + " " + builtinMeta.length);
      }

      AtomConstructor builtin;
      builtin = new AtomConstructor(builtinMeta[0], scope, true);
      if (builtinMeta.length == 2) {
        builtin = builtin.initializeFields();
      } else {
        // there are some type params
        String[] paramNames = builtinMeta[2].split(",");
        ArgumentDefinition[] args = new ArgumentDefinition[paramNames.length];
        for (int i = 0; i < paramNames.length; i++) {
          args[i] = new ArgumentDefinition(i, paramNames[i], ArgumentDefinition.ExecutionMode.EXECUTE);
        }
        builtin = builtin.initializeFields(args);
      }
      return builtin;
    }).filter(b -> b != null).collect(Collectors.toList());
  }
  
  private void readBuiltinsMetadata(ModuleScope scope) {
    ClassLoader classLoader = getClass().getClassLoader();
    List<String> lines;
    try (InputStream resource = classLoader.getResourceAsStream(MethodDefinition.META_PATH)) {
      lines = new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8)).lines().collect(Collectors.toList());
    } catch (Exception ioe) {
      lines = new ArrayList<>();
      ioe.printStackTrace();
    }

    lines.forEach(line -> {
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
          scope.getLocalConstructor(builtinMethodOwner).ifPresentOrElse(constr -> {
            Map<String, Class<BuiltinRootNode>> atomNodes = builtinNodes.get(builtinMethodOwner);
            if (atomNodes == null) {
              atomNodes = new HashMap<>();
              // TODO: move away from String Map once Builtins are gone
              builtinNodes.put(constr.getName(), atomNodes);
            }
            atomNodes.put(builtinMethodName, clazz);
          }, () -> {
            Map<String, Class<BuiltinRootNode>> atomNodes = builtinNodes.get(builtinMethodOwner);
            if (atomNodes == null) {
              atomNodes = new HashMap<>();
              // TODO: move away from String Map once Builtins are gone
              builtinNodes.put(builtinMethodOwner, atomNodes);
            }
            atomNodes.put(builtinMethodName, clazz);
          });
        } catch (ClassNotFoundException e) {
          e.printStackTrace();
        }
    });
  }

  public Optional<Function> getBuiltinFunction(AtomConstructor atom, String methodName, Language language) {
    // TODO: move away from String mapping once Builtins is gone
    Map<String, Class<BuiltinRootNode>> atomNodes = builtinNodes.get(atom.getName());
    if (atomNodes == null)
      return Optional.empty();
    Class<BuiltinRootNode> clazz = atomNodes.get(methodName);
    if (clazz == null)
      return Optional.empty();
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
    return builtinTypes.get(name);
  }

  /**
   * Returns the {@code Nothing} atom constructor.
   *
   * @return the {@code Nothing} atom constructor
   */
  public AtomConstructor nothing() {
    return getBuiltinType(Nothing.class);
  }

  /**
   * Returns the {@code Text} part of builtins.
   *
   * @return the {@code Text} part of builtins.
   */
  public AtomConstructor text() {
    return getBuiltinType(Text.class);
  }

  /**
   * Returns the {@code Function} atom constructor.
   *
   * @return the {@code Function} atom constructor
   */
  public AtomConstructor function() {
    return getBuiltinType(org.enso.interpreter.node.expression.builtin.function.Function.class);
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
    return this.getBuiltinType(Boolean.class);
  }

  /** @return the True constructor. */
  public AtomConstructor trueAtom() {
    return this.getBuiltinType(True.class);
  }

  /** @return the False constructor. */
  public AtomConstructor falseAtom() {
    return this.getBuiltinType(False.class);
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
    return this.getBuiltinType(Any.class);
  }

  /**
   * Returns the {@code Debug} atom constructor.
   *
   * @return the {@code Debug} atom constructor
   */
  public AtomConstructor debug() {
    return debug;
  }

  /** @return the {@code Enso_Project} atom constructor */
  public AtomConstructor getProjectDescription() {
    return projectDescription;
  }

  /** @return the {@code System} atom constructor. */
  public System system() {
    return system;
  }

  /** @return the Array constructor. */
  public AtomConstructor array() {
    return this.getBuiltinType(Array.class);
  }

  /** @return the Ref constructor. */
  public AtomConstructor ref() {
    return this.getBuiltinType(Ref.class);
  }

  /** @return the container for polyglot-related builtins. */
  public AtomConstructor polyglot() {
    return this.getBuiltinType(Polyglot.class);
  }

  /** @return the {@code Caught_Panic} atom constructor */
  public AtomConstructor caughtPanic() {
    return this.getBuiltinType(CaughtPanic.class);
  }

  /** @return the {@code Panic} atom constructor */
  public AtomConstructor panic() {
    return this.getBuiltinType(Panic.class);
  }

  /** @return the container for ordering-related builtins */
  public Ordering ordering() {
    return ordering;
  }

  /** @return the container for the dataflow error-related builtins */
  public AtomConstructor dataflowError() {
    return this.getBuiltinType(org.enso.interpreter.node.expression.builtin.Error.class);
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
   * @param typeName the fully qualified type name as defined in {@link Constants}.
   * @return the associated {@link org.enso.interpreter.runtime.callable.atom.Atom} if it exists,
   *     and {@code null} otherwise
   */
  public Atom fromTypeSystem(String typeName) {
    switch (typeName) {
      case Constants.ANY:
        return any().newInstance();
      case Constants.ARRAY:
        return array().newInstance();
      case Constants.BOOLEAN:
        return bool().newInstance();
      case Constants.DECIMAL:
        return number.getDecimal().newInstance();
      case Constants.ERROR:
        return dataflowError().newInstance();
      case Constants.FUNCTION:
        return function().newInstance();
      case Constants.INTEGER:
        return number.getInteger().newInstance();
      case Constants.MANAGED_RESOURCE:
        return resource.getManagedResource().newInstance();
      case Constants.NOTHING:
        return nothing().newInstance();
      case Constants.NUMBER:
        return number.getNumber().newInstance();
      case Constants.PANIC:
        return panic().newInstance();
      case Constants.REF:
        return ref().newInstance();
      case Constants.TEXT:
        return text().newInstance();
      default:
        return null;
    }
  }
}
