package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointMethodGen;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalMethodGen;
import org.enso.interpreter.node.expression.builtin.error.*;
import org.enso.interpreter.node.expression.builtin.function.ApplicationOperatorMethodGen;
import org.enso.interpreter.node.expression.builtin.function.ExplicitCallFunctionMethodGen;
import org.enso.interpreter.node.expression.builtin.interop.generic.*;
import org.enso.interpreter.node.expression.builtin.interop.syntax.MethodDispatchNode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.ConstructorDispatchNode;
import org.enso.interpreter.node.expression.builtin.io.*;
import org.enso.interpreter.node.expression.builtin.number.*;
import org.enso.interpreter.node.expression.builtin.runtime.GCMethodGen;
import org.enso.interpreter.node.expression.builtin.runtime.NoInlineMethodGen;
import org.enso.interpreter.node.expression.builtin.state.*;
import org.enso.interpreter.node.expression.builtin.interop.java.*;
import org.enso.interpreter.node.expression.builtin.text.*;
import org.enso.interpreter.node.expression.builtin.thread.WithInterruptHandlerMethodGen;
import org.enso.interpreter.node.expression.builtin.unsafe.SetAtomFieldMethodGen;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
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

  private final Module module;
  private final ModuleScope scope;
  private final AtomConstructor unit;
  private final AtomConstructor any;
  private final AtomConstructor number;
  private final AtomConstructor function;
  private final AtomConstructor text;
  private final AtomConstructor debug;
  private final Error error;
  private final Bool bool;
  private final System system;
  private final Array array;

  private final RootCallTarget interopDispatchRoot;
  private final FunctionSchema interopDispatchSchema;
  private final Function newInstanceFunction;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param context the current {@link Context} instance
   */
  public Builtins(Context context) {
    Language language = context.getLanguage();

    module = Module.empty(QualifiedName.fromString(MODULE_NAME).get());
    scope = module.compileScope(context);
    unit = new AtomConstructor("Unit", scope).initializeFields();
    any = new AtomConstructor("Any", scope).initializeFields();
    number = new AtomConstructor("Number", scope).initializeFields();
    bool = new Bool(language, scope);
    error = new Error(language, scope);
    array = new Array(language, scope);
    function = new AtomConstructor("Function", scope).initializeFields();
    text = new AtomConstructor("Text", scope).initializeFields();
    debug = new AtomConstructor("Debug", scope).initializeFields();
    system = new System(language, scope);

    AtomConstructor nil = new AtomConstructor("Nil", scope).initializeFields();
    AtomConstructor cons =
        new AtomConstructor("Cons", scope)
            .initializeFields(
                new ArgumentDefinition(0, "head", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "tail", ArgumentDefinition.ExecutionMode.EXECUTE));
    AtomConstructor io = new AtomConstructor("IO", scope).initializeFields();
    AtomConstructor runtime = new AtomConstructor("Runtime", scope).initializeFields();
    AtomConstructor panic = new AtomConstructor("Panic", scope).initializeFields();
    AtomConstructor error = new AtomConstructor("Error", scope).initializeFields();
    AtomConstructor state = new AtomConstructor("State", scope).initializeFields();

    AtomConstructor java = new AtomConstructor("Java", scope).initializeFields();
    AtomConstructor thread = new AtomConstructor("Thread", scope).initializeFields();

    AtomConstructor unsafe = new AtomConstructor("Unsafe", scope).initializeFields();

    scope.registerConstructor(unit);
    scope.registerConstructor(any);
    scope.registerConstructor(number);
    scope.registerConstructor(function);
    scope.registerConstructor(text);

    scope.registerConstructor(cons);
    scope.registerConstructor(nil);
    scope.registerConstructor(io);
    scope.registerConstructor(panic);
    scope.registerConstructor(error);
    scope.registerConstructor(state);
    scope.registerConstructor(debug);
    scope.registerConstructor(runtime);

    scope.registerConstructor(java);
    scope.registerConstructor(thread);

    scope.registerConstructor(unsafe);

    createPolyglot(language);

    scope.registerMethod(io, "println", PrintlnMethodGen.makeFunction(language));
    scope.registerMethod(io, "print_err", PrintErrMethodGen.makeFunction(language));
    scope.registerMethod(io, "readln", ReadlnMethodGen.makeFunction(language));

    scope.registerMethod(runtime, "no_inline", NoInlineMethodGen.makeFunction(language));
    scope.registerMethod(runtime, "gc", GCMethodGen.makeFunction(language));

    scope.registerMethod(panic, "throw", ThrowPanicMethodGen.makeFunction(language));
    scope.registerMethod(panic, "recover", CatchPanicMethodGen.makeFunction(language));
    scope.registerMethod(error, "throw", ThrowErrorMethodGen.makeFunction(language));
    scope.registerMethod(any, "catch", CatchErrorMethodGen.makeFunction(language));

    scope.registerMethod(number, "+", AddMethodGen.makeFunction(language));
    scope.registerMethod(number, "-", SubtractMethodGen.makeFunction(language));
    scope.registerMethod(number, "*", MultiplyMethodGen.makeFunction(language));
    scope.registerMethod(number, "/", DivideMethodGen.makeFunction(language));
    scope.registerMethod(number, "%", ModMethodGen.makeFunction(language));
    scope.registerMethod(number, "negate", NegateMethodGen.makeFunction(language));
    scope.registerMethod(number, "==", EqualsMethodGen.makeFunction(language));

    scope.registerMethod(state, "get", GetStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "put", PutStateMethodGen.makeFunction(language));
    scope.registerMethod(state, "run", RunStateMethodGen.makeFunction(language));

    scope.registerMethod(debug, MethodNames.Debug.EVAL, DebugEvalMethodGen.makeFunction(language));
    scope.registerMethod(debug, "breakpoint", DebugBreakpointMethodGen.makeFunction(language));

    scope.registerMethod(function, "call", ExplicitCallFunctionMethodGen.makeFunction(language));
    scope.registerMethod(function, "<|", ApplicationOperatorMethodGen.makeFunction(language));

    scope.registerMethod(text, "+", ConcatMethodGen.makeFunction(language));
    scope.registerMethod(text, "==", TextEqualsMethodGen.makeFunction(language));
    scope.registerMethod(any, "to_text", AnyToTextMethodGen.makeFunction(language));
    scope.registerMethod(any, "json_serialize", JsonSerializeMethodGen.makeFunction(language));

    scope.registerMethod(java, "add_to_class_path", AddToClassPathMethodGen.makeFunction(language));
    scope.registerMethod(java, "lookup_class", LookupClassMethodGen.makeFunction(language));

    scope.registerMethod(
        thread, "with_interrupt_handler", WithInterruptHandlerMethodGen.makeFunction(language));

    scope.registerMethod(unsafe, "set_atom_field", SetAtomFieldMethodGen.makeFunction(language));

    interopDispatchRoot = Truffle.getRuntime().createCallTarget(MethodDispatchNode.build(language));
    interopDispatchSchema =
        new FunctionSchema(
            FunctionSchema.CallStrategy.ALWAYS_DIRECT,
            FunctionSchema.CallerFrameAccess.NONE,
            new ArgumentDefinition[] {
              new ArgumentDefinition(1, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
              new ArgumentDefinition(2, "method_name", ArgumentDefinition.ExecutionMode.EXECUTE),
              new ArgumentDefinition(3, "arguments", ArgumentDefinition.ExecutionMode.EXECUTE)
            },
            new boolean[] {false, true, false},
            new CallArgumentInfo[0]);
    newInstanceFunction = ConstructorDispatchNode.makeFunction(language);

    module.unsafeBuildIrStub();
  }

  private void createPolyglot(Language language) {
    AtomConstructor polyglot = new AtomConstructor("Polyglot", scope).initializeFields();
    scope.registerConstructor(polyglot);
    scope.registerMethod(polyglot, "execute", ExecuteMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "invoke", InvokeMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "new", InstantiateMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_member", GetMemberMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_members", GetMembersMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_array_size", GetArraySizeMethodGen.makeFunction(language));
    scope.registerMethod(
        polyglot, "get_array_element", GetArrayElementMethodGen.makeFunction(language));
  }

  /**
   * Returns the {@code Unit} atom constructor.
   *
   * @return the {@code Unit} atom constructor
   */
  public AtomConstructor unit() {
    return unit;
  }

  /**
   * Returns the {@code Text} atom constructor.
   *
   * @return the {@code Text} atom constructor
   */
  public AtomConstructor text() {
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
   * Returns the {@code Number} atom constructor.
   *
   * @return the {@code Number} atom constructor
   */
  public AtomConstructor number() {
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

  /** @return the {@code System} atom constructor. */
  public System system() {
    return system;
  }

  /** @return the container for array-related builtins. */
  public Array array() {
    return array;
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
   * Builds a function dispatching to a polyglot method call.
   *
   * @param method the name and scope of the method this function will dispatch to.
   * @return a function calling {@code method} with given arguments.
   */
  public Function buildPolyglotMethodDispatch(UnresolvedSymbol method) {
    Object[] preAppliedArr = new Object[] {null, method, null};
    return new Function(interopDispatchRoot, null, interopDispatchSchema, preAppliedArr, null);
  }

  /** @return a function executing a constructor with given arguments. */
  public Function getConstructorDispatch() {
    return newInstanceFunction;
  }
}
