package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

import org.enso.compiler.Passes;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.exception.CompilerError;
import org.enso.compiler.phase.BuiltinsIrBuilder;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.model.MethodDefinition;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointMethodGen;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalMethodGen;
import org.enso.interpreter.node.expression.builtin.error.CatchPanicMethodGen;
import org.enso.interpreter.node.expression.builtin.error.CaughtPanicConvertToDataflowErrorMethodGen;
import org.enso.interpreter.node.expression.builtin.error.GetAttachedStackTraceMethodGen;
import org.enso.interpreter.node.expression.builtin.error.ThrowPanicMethodGen;
import org.enso.interpreter.node.expression.builtin.function.ExplicitCallFunctionMethodGen;
import org.enso.interpreter.node.expression.builtin.io.GetCwdMethodGen;
import org.enso.interpreter.node.expression.builtin.io.GetFileMethodGen;
import org.enso.interpreter.node.expression.builtin.io.GetUserHomeMethodGen;
import org.enso.interpreter.node.expression.builtin.io.PrintErrMethodGen;
import org.enso.interpreter.node.expression.builtin.io.PrintlnMethodGen;
import org.enso.interpreter.node.expression.builtin.io.ReadlnMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.GCMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.NoInlineMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.NoInlineWithArgMethodGen;
import org.enso.interpreter.node.expression.builtin.state.GetStateMethodGen;
import org.enso.interpreter.node.expression.builtin.state.PutStateMethodGen;
import org.enso.interpreter.node.expression.builtin.state.RunStateMethodGen;
import org.enso.interpreter.node.expression.builtin.thread.WithInterruptHandlerMethodGen;
import org.enso.interpreter.node.expression.builtin.unsafe.SetAtomFieldMethodGen;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.BuiltinAtomConstructor;
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
  private HashMap<String, BuiltinAtomConstructor> builtinTypes;

  private final AtomConstructor debug;
  private final AtomConstructor projectDescription;
  private final AtomConstructor function;
  private final AtomConstructor nothing;
  private final AtomConstructor panic;
  private final AtomConstructor caughtPanic;

  private final DataflowError dataflowError;
  private final Error error;
  private final Module module;
  private final ModuleScope scope;
  private final Number number;
  private final Ordering ordering;
  private final Resource resource;
  private final System system;
  private final Text text;
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
    dataflowError = new DataflowError(language, scope);
    Warning.initWarningMethods(language, scope);
    projectDescription =
        new AtomConstructor("Project_Description", scope)
            .initializeFields(
                new ArgumentDefinition(
                    0, "prim_root_file", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "prim_config", ArgumentDefinition.ExecutionMode.EXECUTE));
    error = new Error(language, scope);
    function = new AtomConstructor("Function", scope).initializeFields();
    nothing = new AtomConstructor("Nothing", scope).initializeFields();
    number = new Number(language, scope);
    ordering = new Ordering(language, scope);
    panic = new AtomConstructor("Panic", scope).initializeFields();
    caughtPanic =
        new AtomConstructor("Caught_Panic", scope)
            .initializeFields(
                new ArgumentDefinition(0, "payload", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(
                    1, "internal_original_exception", ArgumentDefinition.ExecutionMode.EXECUTE));
    resource = new Resource(language, scope);
    system = new System(language, scope);
    text = new Text(language, scope);
    special = new Special(language);

    AtomConstructor nil = new AtomConstructor("Nil", scope).initializeFields();
    AtomConstructor cons =
        new AtomConstructor("Cons", scope)
            .initializeFields(
                new ArgumentDefinition(0, "head", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "tail", ArgumentDefinition.ExecutionMode.EXECUTE));
    AtomConstructor io = new AtomConstructor("IO", scope).initializeFields();
    AtomConstructor primIo = new AtomConstructor("Prim_Io", scope).initializeFields();
    AtomConstructor runtime = new AtomConstructor("Runtime", scope).initializeFields();
    AtomConstructor state = new AtomConstructor("State", scope).initializeFields();

    AtomConstructor thread = new AtomConstructor("Thread", scope).initializeFields();

    AtomConstructor unsafe = new AtomConstructor("Unsafe", scope).initializeFields();
    scope.registerConstructor(nothing);
    scope.registerConstructor(function);

    scope.registerConstructor(cons);
    scope.registerConstructor(nil);
    scope.registerConstructor(io);
    scope.registerConstructor(primIo);
    scope.registerConstructor(panic);
    scope.registerConstructor(caughtPanic);
    scope.registerConstructor(state);
    scope.registerConstructor(debug);
    scope.registerConstructor(projectDescription);
    scope.registerConstructor(runtime);
    scope.registerConstructor(thread);

    scope.registerConstructor(unsafe);

    scope.registerMethod(io, "println", PrintlnMethodGen.makeFunction(language));
    scope.registerMethod(io, "print_err", PrintErrMethodGen.makeFunction(language));
    scope.registerMethod(io, "readln", ReadlnMethodGen.makeFunction(language));
    scope.registerMethod(primIo, "get_file", GetFileMethodGen.makeFunction(language));
    scope.registerMethod(primIo, "get_cwd", GetCwdMethodGen.makeFunction(language));
    scope.registerMethod(primIo, "get_user_home", GetUserHomeMethodGen.makeFunction(language));

    scope.registerMethod(runtime, "no_inline", NoInlineMethodGen.makeFunction(language));
    scope.registerMethod(
        runtime, "no_inline_with_arg", NoInlineWithArgMethodGen.makeFunction(language));
    scope.registerMethod(runtime, "gc", GCMethodGen.makeFunction(language));
    scope.registerMethod(
        runtime, "primitive_get_stack_trace", GetStackTraceMethodGen.makeFunction(language));

    scope.registerMethod(panic, "throw", ThrowPanicMethodGen.makeFunction(language));
    scope.registerMethod(panic, "catch_primitive", CatchPanicMethodGen.makeFunction(language));
    scope.registerMethod(
        panic,
        "primitive_get_attached_stack_trace",
        GetAttachedStackTraceMethodGen.makeFunction(language));
    scope.registerMethod(caughtPanic, "convert_to_dataflow_error", CaughtPanicConvertToDataflowErrorMethodGen.makeFunction(language));
    scope.registerMethod(state, "get", GetStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "put", PutStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "run", RunStateMethodGen.makeFunction(language));

    scope.registerMethod(debug, MethodNames.Debug.EVAL, DebugEvalMethodGen.makeFunction(language));
    scope.registerMethod(debug, "breakpoint", DebugBreakpointMethodGen.makeFunction(language));

    scope.registerMethod(function, "call", ExplicitCallFunctionMethodGen.makeFunction(language));
    scope.registerMethod(
        thread, "with_interrupt_handler", WithInterruptHandlerMethodGen.makeFunction(language));

    scope.registerMethod(unsafe, "set_atom_field", SetAtomFieldMethodGen.makeFunction(language));
    readBuiltinsMetadata(scope);

    // FIXME: should be possible to get rid of hardcoded list of builtin types once we have all of them
    //        stored in metadata files
    List<String> builtinConstructors = new ArrayList<>();
    builtinConstructors.add("Polyglot");
    builtinConstructors.add("Ref");
    builtinConstructors.add("Array");
    builtinConstructors.add("Any");
    builtinConstructors.add("Boolean");
    builtinConstructors.add("True");
    builtinConstructors.add("False");
    initBuiltinTypes(builtinConstructors, scope, language);
  }

  public void initBuiltinTypes(List<String> constrs, ModuleScope scope, Language language) {
    for (String constr: constrs) {
      BuiltinAtomConstructor atom = new BuiltinAtomConstructor(constr, scope).initializeFields();
      builtinTypes.put(constr, atom);
      Map<String, Class<BuiltinRootNode>> methods = builtinNodes.get(constr);
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

  private void readBuiltinsMetadata(ModuleScope scope) {
    ClassLoader classLoader = getClass().getClassLoader();
    FileSystem fs = null;
    Stream<Path> builtinMetaPath;
    try {
      URI resource = classLoader.getResource(MethodDefinition.META_PATH).toURI();
      fs = initFileSystem(resource);
      builtinMetaPath = Files.walk(Paths.get(resource)).flatMap(p -> acceptMetadataFiles(p));
    } catch (Exception ioe) {
      ioe.printStackTrace();
      builtinMetaPath = Stream.empty();
    }
    builtinMetaPath.forEach(metaPath -> {
      List<String> lines;
      try {
        lines = Files.readAllLines(metaPath, StandardCharsets.UTF_8);
      } catch (IOException e) {
        e.printStackTrace();
        lines = new ArrayList<>();
      }
      lines.forEach(line -> {
          String[] builtinMeta = line.split(":");
          if (builtinMeta.length != 2) {
            throw new CompilerError("Invalid builtin metadata in " + metaPath + ": " + line);
          }
          String[] builtinName = builtinMeta[0].split("\\.");
          if (builtinName.length != 2) {
            throw new CompilerError("Invalid builtin metadata in " + metaPath + ": " + line);
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
    });
    if (fs != null) {
      try {
        fs.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  private FileSystem initFileSystem(URI uri) throws IOException
  {
    // Returning null ensures that we use the default one and at the same time we don't attempt
    // to close it.
    try {
      FileSystems.getFileSystem(uri);
      return null;
    } catch (IllegalArgumentException iae) {
      // file: schema doesn't like non-/ path but that's fine, it means the default file system is already setup
      return null;
    } catch (FileSystemNotFoundException e)  {
      Map<String, String> env = new HashMap<>();
      env.put("create", "true");
      return FileSystems.newFileSystem(uri, env);
    }
  }

  private Stream<Path> acceptMetadataFiles(Path path) {
    if (Files.isRegularFile(path) && path.getFileName().toString().endsWith(MethodDefinition.META_BUILTIN_EXTENSION)) {
      return Stream.of(path);
    }
    return Stream.empty();
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

  public BuiltinAtomConstructor getBuiltinType(String name) {
    return builtinTypes.get(name);
  }

  /**
   * Returns the {@code Nothing} atom constructor.
   *
   * @return the {@code Nothing} atom constructor
   */
  public AtomConstructor nothing() {
    return nothing;
  }

  /**
   * Returns the {@code Text} part of builtins.
   *
   * @return the {@code Text} part of builtins.
   */
  public Text text() {
    return text;
  }

  /**
   * Returns the {@code Function} atom constructor.
   *
   * @return the {@code Function} atom constructor
   */
  public AtomConstructor function() {
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
    return builtinTypes.get("Boolean");
  }

  /** @return the True constructor. */
  public AtomConstructor trueAtom() {
    return builtinTypes.get("True");
  }

  /** @return the False constructor. */
  public AtomConstructor falseAtom() {
    return builtinTypes.get("False");
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
    return this.getBuiltinType("Any");
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
    return builtinTypes.get("Array");
  }

  /** @return the Ref constructor. */
  public AtomConstructor ref() {
    return builtinTypes.get("Ref");
  }

  /** @return the container for polyglot-related builtins. */
  public AtomConstructor polyglot() {
    return builtinTypes.get("Polyglot");
  }

  /** @return the {@code Caught_Panic} atom constructor */
  public AtomConstructor caughtPanic() {
    return caughtPanic;
  }

  /** @return the container for ordering-related builtins */
  public Ordering ordering() {
    return ordering;
  }

  /** @return the container for the dataflow error-related builtins */
  public DataflowError dataflowError() {
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
        return dataflowError.constructor().newInstance();
      case Constants.FUNCTION:
        return function.newInstance();
      case Constants.INTEGER:
        return number.getInteger().newInstance();
      case Constants.MANAGED_RESOURCE:
        return resource.getManagedResource().newInstance();
      case Constants.NOTHING:
        return nothing.newInstance();
      case Constants.NUMBER:
        return number.getNumber().newInstance();
      case Constants.PANIC:
        return panic.newInstance();
      case Constants.REF:
        return ref().newInstance();
      case Constants.TEXT:
        return text.getText().newInstance();
      default:
        return null;
    }
  }
}
