package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointMethodGen;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalMethodGen;
import org.enso.interpreter.node.expression.builtin.error.CatchErrorMethodGen;
import org.enso.interpreter.node.expression.builtin.error.CatchPanicMethodGen;
import org.enso.interpreter.node.expression.builtin.error.ThrowErrorMethodGen;
import org.enso.interpreter.node.expression.builtin.error.ThrowPanicMethodGen;
import org.enso.interpreter.node.expression.builtin.function.ApplicationOperatorMethodGen;
import org.enso.interpreter.node.expression.builtin.function.ExplicitCallFunctionMethodGen;
import org.enso.interpreter.node.expression.builtin.interop.java.AddToClassPathMethodGen;
import org.enso.interpreter.node.expression.builtin.interop.java.LookupClassMethodGen;
import org.enso.interpreter.node.expression.builtin.io.*;
import org.enso.interpreter.node.expression.builtin.runtime.GCMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.NoInlineMethodGen;
import org.enso.interpreter.node.expression.builtin.state.GetStateMethodGen;
import org.enso.interpreter.node.expression.builtin.state.PutStateMethodGen;
import org.enso.interpreter.node.expression.builtin.state.RunStateMethodGen;
import org.enso.interpreter.node.expression.builtin.text.AnyToTextMethodGen;
import org.enso.interpreter.node.expression.builtin.thread.WithInterruptHandlerMethodGen;
import org.enso.interpreter.node.expression.builtin.unsafe.SetAtomFieldMethodGen;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  public static final String MODULE_NAME = "Builtins.Main";

  /** Container for method names needed outside this class. */
  public static class MethodNames {
    public static class Debug {
      public static final String EVAL = "eval";
    }
  }

  private final AtomConstructor any;
  private final AtomConstructor debug;
  private final AtomConstructor ensoProject;
  private final AtomConstructor function;
  private final AtomConstructor nothing;

  private final Bool bool;
  private final Error error;
  private final Meta meta;
  private final Module module;
  private final ModuleScope scope;
  private final Mutable mutable;
  private final Number number;
  private final Ordering ordering;
  private final Polyglot polyglot;
  private final Resource resource;
  private final System system;
  private final Text text;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param context the current {@link Context} instance
   */
  public Builtins(Context context) {
    Language language = context.getLanguage();
    module = Module.empty(QualifiedName.fromString(MODULE_NAME));
    scope = module.compileScope(context);

    any = new AtomConstructor("Any", scope).initializeFields();
    bool = new Bool(language, scope);
    debug = new AtomConstructor("Debug", scope).initializeFields();
    ensoProject =
        new AtomConstructor("Enso_Project", scope)
            .initializeFields(
                new ArgumentDefinition(
                    0, "prim_root_file", ArgumentDefinition.ExecutionMode.EXECUTE));
    error = new Error(language, scope);
    function = new AtomConstructor("Function", scope).initializeFields();
    meta = new Meta(language, scope);
    mutable = new Mutable(language, scope);
    nothing = new AtomConstructor("Nothing", scope).initializeFields();
    number = new Number(language, scope);
    ordering = new Ordering(language, scope);
    polyglot = new Polyglot(language, scope);
    resource = new Resource(language, scope);
    system = new System(language, scope);
    text = new Text(language, scope);

    AtomConstructor nil = new AtomConstructor("Nil", scope).initializeFields();
    AtomConstructor cons =
        new AtomConstructor("Cons", scope)
            .initializeFields(
                new ArgumentDefinition(0, "head", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "tail", ArgumentDefinition.ExecutionMode.EXECUTE));
    AtomConstructor io = new AtomConstructor("IO", scope).initializeFields();
    AtomConstructor primIo = new AtomConstructor("Prim_Io", scope).initializeFields();
    AtomConstructor runtime = new AtomConstructor("Runtime", scope).initializeFields();
    AtomConstructor panic = new AtomConstructor("Panic", scope).initializeFields();
    AtomConstructor error = new AtomConstructor("Error", scope).initializeFields();
    AtomConstructor state = new AtomConstructor("State", scope).initializeFields();

    AtomConstructor java = new AtomConstructor("Java", scope).initializeFields();
    AtomConstructor thread = new AtomConstructor("Thread", scope).initializeFields();

    AtomConstructor unsafe = new AtomConstructor("Unsafe", scope).initializeFields();
    scope.registerConstructor(nothing);
    scope.registerConstructor(any);
    scope.registerConstructor(function);

    scope.registerConstructor(cons);
    scope.registerConstructor(nil);
    scope.registerConstructor(io);
    scope.registerConstructor(primIo);
    scope.registerConstructor(panic);
    scope.registerConstructor(error);
    scope.registerConstructor(state);
    scope.registerConstructor(debug);
    scope.registerConstructor(ensoProject);
    scope.registerConstructor(runtime);

    scope.registerConstructor(java);
    scope.registerConstructor(thread);

    scope.registerConstructor(unsafe);

    scope.registerMethod(io, "println", PrintlnMethodGen.makeFunction(language));
    scope.registerMethod(io, "print_err", PrintErrMethodGen.makeFunction(language));
    scope.registerMethod(io, "readln", ReadlnMethodGen.makeFunction(language));
    scope.registerMethod(primIo, "get_file", GetFileMethodGen.makeFunction(language));
    scope.registerMethod(primIo, "get_cwd", GetCwdMethodGen.makeFunction(language));
    scope.registerMethod(primIo, "get_user_home", GetUserHomeMethodGen.makeFunction(language));

    scope.registerMethod(runtime, "no_inline", NoInlineMethodGen.makeFunction(language));
    scope.registerMethod(runtime, "gc", GCMethodGen.makeFunction(language));

    scope.registerMethod(panic, "throw", ThrowPanicMethodGen.makeFunction(language));
    scope.registerMethod(panic, "recover", CatchPanicMethodGen.makeFunction(language));
    scope.registerMethod(error, "throw", ThrowErrorMethodGen.makeFunction(language));
    scope.registerMethod(any, "catch", CatchErrorMethodGen.makeFunction(language));

    scope.registerMethod(state, "get", GetStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "put", PutStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "run", RunStateMethodGen.makeFunction(language));

    scope.registerMethod(debug, MethodNames.Debug.EVAL, DebugEvalMethodGen.makeFunction(language));
    scope.registerMethod(debug, "breakpoint", DebugBreakpointMethodGen.makeFunction(language));

    scope.registerMethod(function, "call", ExplicitCallFunctionMethodGen.makeFunction(language));
    scope.registerMethod(function, "<|", ApplicationOperatorMethodGen.makeFunction(language));

    scope.registerMethod(any, "to_text", AnyToTextMethodGen.makeFunction(language));

    scope.registerMethod(java, "add_to_class_path", AddToClassPathMethodGen.makeFunction(language));
    scope.registerMethod(java, "lookup_class", LookupClassMethodGen.makeFunction(language));

    scope.registerMethod(
        thread, "with_interrupt_handler", WithInterruptHandlerMethodGen.makeFunction(language));

    scope.registerMethod(unsafe, "set_atom_field", SetAtomFieldMethodGen.makeFunction(language));

    module.unsafeBuildIrStub();
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

  /** @return the Boolean part of builtins. */
  public Bool bool() {
    return bool;
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
    return any;
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
  public AtomConstructor getEnsoProject() {
    return ensoProject;
  }

  /** @return the {@code System} atom constructor. */
  public System system() {
    return system;
  }

  /** @return the container for mutable memory related builtins. */
  public Mutable mutable() {
    return mutable;
  }

  /** @return the container for polyglot-related builtins. */
  public Polyglot polyglot() {
    return polyglot;
  }

  /** @return the container for ordering-related builtins */
  public Ordering ordering() {
    return ordering;
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
}
