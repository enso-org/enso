package org.enso.interpreter.runtime;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.IfZeroNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugEvalNode;
import org.enso.interpreter.node.expression.builtin.error.CatchErrorNode;
import org.enso.interpreter.node.expression.builtin.error.CatchPanicNode;
import org.enso.interpreter.node.expression.builtin.error.PanicNode;
import org.enso.interpreter.node.expression.builtin.error.ThrowErrorNode;
import org.enso.interpreter.node.expression.builtin.io.PrintNode;
import org.enso.interpreter.node.expression.builtin.state.GetStateNode;
import org.enso.interpreter.node.expression.builtin.state.PutStateNode;
import org.enso.interpreter.node.expression.builtin.state.RunStateNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Container class for static predefined atoms, methods, and their containing scope. */
public class Builtins {
  private final ModuleScope scope;
  private final AtomConstructor unit;

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param language the current {@link Language} instance
   */
  public Builtins(Language language) {
    scope = new ModuleScope();
    unit = new AtomConstructor("Unit", scope).initializeFields();

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
    AtomConstructor debug = new AtomConstructor("Debug", scope).initializeFields();

    scope.registerConstructor(cons);
    scope.registerConstructor(nil);
    scope.registerConstructor(unit);
    scope.registerConstructor(io);
    scope.registerConstructor(panic);
    scope.registerConstructor(error);
    scope.registerConstructor(state);
    scope.registerConstructor(debug);

    scope.registerMethod(io, "println", PrintNode.makeFunction(language));

    scope.registerMethod(panic, "throw", PanicNode.makeFunction(language));
    scope.registerMethod(panic, "recover", CatchPanicNode.makeFunction(language));
    scope.registerMethod(error, "throw", ThrowErrorNode.makeFunction(language));
    scope.registerMethodForAny("catch", CatchErrorNode.makeFunction(language));

    scope.registerMethodForNumber("ifZero", IfZeroNode.makeFunction(language));

    scope.registerMethod(state, "get", GetStateNode.makeFunction(language));
    scope.registerMethod(state, "put", PutStateNode.makeFunction(language));
    scope.registerMethod(state, "run", RunStateNode.makeFunction(language));

    scope.registerMethod(debug, "eval", DebugEvalNode.makeFunction(language));
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
   * Returns the builtin module scope.
   *
   * @return the builtin module scope
   */
  public ModuleScope getScope() {
    return scope;
  }
}
