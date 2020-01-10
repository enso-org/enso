package org.enso.interpreter.runtime;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.IfZeroNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalNode;
import org.enso.interpreter.node.expression.builtin.error.CatchErrorNode;
import org.enso.interpreter.node.expression.builtin.error.CatchPanicNode;
import org.enso.interpreter.node.expression.builtin.error.PanicNode;
import org.enso.interpreter.node.expression.builtin.error.ThrowErrorNode;
import org.enso.interpreter.node.expression.builtin.function.ExplicitCallFunctionNode;
import org.enso.interpreter.node.expression.builtin.io.PrintNode;
import org.enso.interpreter.node.expression.builtin.state.GetStateNode;
import org.enso.interpreter.node.expression.builtin.state.PutStateNode;
import org.enso.interpreter.node.expression.builtin.state.RunStateNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  public static final String MODULE_NAME = "Builtins";

  /** Container for method names needed outside this class. */
  public static class MethodNames {
    public static class Debug {
      public static final String EVAL = "eval";
    }
  }

  private final ModuleScope scope;
  private final AtomConstructor unit;
  private final AtomConstructor any;
  private final AtomConstructor number;
  private final AtomConstructor function;
  private final AtomConstructor text;
  private final AtomConstructor debug;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param language the current {@link Language} instance
   */
  public Builtins(Language language) {
    scope = new ModuleScope(MODULE_NAME);
    unit = new AtomConstructor("Unit", scope).initializeFields();
    any = new AtomConstructor("Any", scope).initializeFields();
    number = new AtomConstructor("Number", scope).initializeFields();
    function = new AtomConstructor("Function", scope).initializeFields();
    text = new AtomConstructor("Text", scope).initializeFields();
    debug = new AtomConstructor("Debug", scope).initializeFields();

    AtomConstructor nil = new AtomConstructor("Nil", scope).initializeFields();
    AtomConstructor cons =
        new AtomConstructor("Cons", scope)
            .initializeFields(
                new ArgumentDefinition(0, "head", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "rest", ArgumentDefinition.ExecutionMode.EXECUTE));
    AtomConstructor io = new AtomConstructor("IO", scope).initializeFields();
    AtomConstructor panic = new AtomConstructor("Panic", scope).initializeFields();
    AtomConstructor error = new AtomConstructor("Error", scope).initializeFields();
    AtomConstructor state = new AtomConstructor("State", scope).initializeFields();

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

    scope.registerMethod(io, "println", PrintNode.makeFunction(language));

    scope.registerMethod(panic, "throw", PanicNode.makeFunction(language));
    scope.registerMethod(panic, "recover", CatchPanicNode.makeFunction(language));
    scope.registerMethod(error, "throw", ThrowErrorNode.makeFunction(language));
    scope.registerMethod(any, "catch", CatchErrorNode.makeFunction(language));

    scope.registerMethod(number, "ifZero", IfZeroNode.makeFunction(language));

    scope.registerMethod(state, "get", GetStateNode.makeFunction(language));
    scope.registerMethod(state, "put", PutStateNode.makeFunction(language));
    scope.registerMethod(state, "run", RunStateNode.makeFunction(language));

    scope.registerMethod(debug, MethodNames.Debug.EVAL, DebugEvalNode.makeFunction(language));
    scope.registerMethod(debug, "breakpoint", DebugBreakpointNode.makeFunction(language));

    scope.registerMethod(function, "call", ExplicitCallFunctionNode.makeFunction(language));
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

  /**
   * Returns the builtin module scope.
   *
   * @return the builtin module scope
   */
  public ModuleScope getScope() {
    return scope;
  }
}
