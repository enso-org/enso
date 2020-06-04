package org.enso.interpreter.runtime;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.IfZeroNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalNode;
import org.enso.interpreter.node.expression.builtin.error.CatchErrorNode;
import org.enso.interpreter.node.expression.builtin.error.CatchPanicNode;
import org.enso.interpreter.node.expression.builtin.error.PanicNode;
import org.enso.interpreter.node.expression.builtin.error.ThrowErrorNode;
import org.enso.interpreter.node.expression.builtin.function.ExplicitCallFunctionNode;
import org.enso.interpreter.node.expression.builtin.interop.generic.*;
import org.enso.interpreter.node.expression.builtin.interop.syntax.MethodDispatchNode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.ConstructorDispatchNode;
import org.enso.interpreter.node.expression.builtin.io.PrintErrNode;
import org.enso.interpreter.node.expression.builtin.io.PrintlnNode;
import org.enso.interpreter.node.expression.builtin.io.ReadlnNode;
import org.enso.interpreter.node.expression.builtin.system.NanoTimeNode;
import org.enso.interpreter.node.expression.builtin.interop.java.*;
import org.enso.interpreter.node.expression.builtin.number.AddNode;
import org.enso.interpreter.node.expression.builtin.number.DivideNode;
import org.enso.interpreter.node.expression.builtin.number.ModNode;
import org.enso.interpreter.node.expression.builtin.number.MultiplyNode;
import org.enso.interpreter.node.expression.builtin.number.NegateNode;
import org.enso.interpreter.node.expression.builtin.number.SubtractNode;
import org.enso.interpreter.node.expression.builtin.state.GetStateNode;
import org.enso.interpreter.node.expression.builtin.state.PutStateNode;
import org.enso.interpreter.node.expression.builtin.state.RunStateNode;
import org.enso.interpreter.node.expression.builtin.text.AnyToTextNode;
import org.enso.interpreter.node.expression.builtin.text.ConcatNode;
import org.enso.interpreter.node.expression.builtin.text.JsonSerializeNode;
import org.enso.interpreter.node.expression.builtin.thread.WithInterruptHandlerNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  public static final String MODULE_NAME = "Builtins";

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
  private final AtomConstructor syntaxError;
  private final AtomConstructor compileError;
  private final AtomConstructor inexhaustivePatternMatchError;

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

    module = Module.empty(QualifiedName.simpleName(MODULE_NAME));
    scope = module.parseScope(context);
    unit = new AtomConstructor("Unit", scope).initializeFields();
    any = new AtomConstructor("Any", scope).initializeFields();
    number = new AtomConstructor("Number", scope).initializeFields();
    function = new AtomConstructor("Function", scope).initializeFields();
    text = new AtomConstructor("Text", scope).initializeFields();
    debug = new AtomConstructor("Debug", scope).initializeFields();
    syntaxError =
        new AtomConstructor("Syntax_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    compileError =
        new AtomConstructor("Compile_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    inexhaustivePatternMatchError =
        new AtomConstructor("Inexhaustive_Pattern_Match_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "scrutinee", ArgumentDefinition.ExecutionMode.EXECUTE));

    AtomConstructor nil = new AtomConstructor("Nil", scope).initializeFields();
    AtomConstructor cons =
        new AtomConstructor("Cons", scope)
            .initializeFields(
                new ArgumentDefinition(0, "head", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "rest", ArgumentDefinition.ExecutionMode.EXECUTE));
    AtomConstructor io = new AtomConstructor("IO", scope).initializeFields();
    AtomConstructor system = new AtomConstructor("System", scope).initializeFields();
    AtomConstructor panic = new AtomConstructor("Panic", scope).initializeFields();
    AtomConstructor error = new AtomConstructor("Error", scope).initializeFields();
    AtomConstructor state = new AtomConstructor("State", scope).initializeFields();

    AtomConstructor java = new AtomConstructor("Java", scope).initializeFields();
    AtomConstructor thread = new AtomConstructor("Thread", scope).initializeFields();

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

    scope.registerConstructor(syntaxError);
    scope.registerConstructor(compileError);

    scope.registerConstructor(java);
    scope.registerConstructor(thread);
    scope.registerConstructor(createPolyglot(language));

    scope.registerMethod(io, "println", PrintlnNode.makeFunction(language));
    scope.registerMethod(io, "print_err", PrintErrNode.makeFunction(language));
    scope.registerMethod(io, "readln", ReadlnNode.makeFunction(language));

    scope.registerMethod(system, "nano_time", NanoTimeNode.makeFunction(language));

    scope.registerMethod(panic, "throw", PanicNode.makeFunction(language));
    scope.registerMethod(panic, "recover", CatchPanicNode.makeFunction(language));
    scope.registerMethod(error, "throw", ThrowErrorNode.makeFunction(language));
    scope.registerMethod(any, "catch", CatchErrorNode.makeFunction(language));

    scope.registerMethod(number, "ifZero", IfZeroNode.makeFunction(language));
    scope.registerMethod(number, "+", AddNode.makeFunction(language));
    scope.registerMethod(number, "-", SubtractNode.makeFunction(language));
    scope.registerMethod(number, "*", MultiplyNode.makeFunction(language));
    scope.registerMethod(number, "/", DivideNode.makeFunction(language));
    scope.registerMethod(number, "%", ModNode.makeFunction(language));
    scope.registerMethod(number, "negate", NegateNode.makeFunction(language));

    scope.registerMethod(state, "get", GetStateNode.makeFunction(language));
    scope.registerMethod(state, "put", PutStateNode.makeFunction(language));
    scope.registerMethod(state, "run", RunStateNode.makeFunction(language));

    scope.registerMethod(debug, MethodNames.Debug.EVAL, DebugEvalNode.makeFunction(language));
    scope.registerMethod(debug, "breakpoint", DebugBreakpointNode.makeFunction(language));

    scope.registerMethod(function, "call", ExplicitCallFunctionNode.makeFunction(language));

    scope.registerMethod(text, "+", ConcatNode.makeFunction(language));
    scope.registerMethod(any, "to_text", AnyToTextNode.makeFunction(language));
    scope.registerMethod(any, "json_serialize", JsonSerializeNode.makeFunction(language));

    scope.registerMethod(java, "add_to_class_path", AddToClassPathNode.makeFunction(language));
    scope.registerMethod(java, "lookup_class", LookupClassNode.makeFunction(language));

    scope.registerMethod(
        thread, "with_interrupt_handler", WithInterruptHandlerNode.makeFunction(language));

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
  }

  private AtomConstructor createPolyglot(Language language) {
    AtomConstructor polyglot = new AtomConstructor("Polyglot", scope).initializeFields();
    scope.registerMethod(polyglot, "execute", ExecuteNode.makeFunction(language));
    scope.registerMethod(polyglot, "invoke", InvokeNode.makeFunction(language));
    scope.registerMethod(polyglot, "new", InstantiateNode.makeFunction(language));
    scope.registerMethod(polyglot, "get_member", GetMemberNode.makeFunction(language));
    scope.registerMethod(polyglot, "get_members", GetMembersNode.makeFunction(language));
    scope.registerMethod(polyglot, "get_array_size", GetArraySizeNode.makeFunction(language));
    scope.registerMethod(polyglot, "get_array_element", GetArrayElementNode.makeFunction(language));
    return polyglot;
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

  /** @return the builtin {@code Syntax_Error} atom constructor. */
  public AtomConstructor syntaxError() {
    return syntaxError;
  }

  /** @return the builtin {@code Compile_Error} atom constructor. */
  public AtomConstructor compileError() {
    return compileError;
  }

  /** @return the builtin {@code Inexhaustive_Pattern_Match_Error} atom constructor. */
  public AtomConstructor inexhaustivePatternMatchError() {
    return inexhaustivePatternMatchError;
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
   * @param method the name of the method this function will dispatch to.
   * @return a function calling {@code method} with given arguments.
   */
  public Function buildPolyglotMethodDispatch(String method) {
    Object[] preAppliedArr = new Object[] {null, method, null};
    return new Function(interopDispatchRoot, null, interopDispatchSchema, preAppliedArr, null);
  }

  /** @return a function executing a constructor with given arguments. */
  public Function getConstructorDispatch() {
    return newInstanceFunction;
  }
}
