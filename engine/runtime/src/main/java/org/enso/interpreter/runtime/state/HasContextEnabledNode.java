package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

/**
 * A node representing the functionality done by {@code Standard.Base.Runtime.Context.is_enabled}.
 */
@GenerateUncached
public abstract class HasContextEnabledNode extends Node {

  public static HasContextEnabledNode getUncached() {
    return HasContextEnabledNodeGen.getUncached();
  }

  public static HasContextEnabledNode create() {
    return HasContextEnabledNodeGen.create();
  }

  /**
   * Returns true if the context represented by the given {@code runtimeCtxCtor} is enabled in the
   * given {@code executionEnvironment}.
   *
   * @param runtimeCtxCtor Constructor of {@code Standard.Base.Runtime.Context}.
   */
  public abstract boolean executeHasContextEnabled(
      ExecutionEnvironment executionEnvironment, AtomConstructor runtimeCtxCtor);

  @Specialization
  boolean doIt(ExecutionEnvironment executionEnvironment, AtomConstructor runtimeCtxCtor) {
    var ensoCtx = EnsoContext.get(this);
    var contextBuiltin = ensoCtx.getBuiltins().context();
    if (runtimeCtxCtor == contextBuiltin.getInput()) {
      return executionEnvironment.permissions.input();
    } else if (runtimeCtxCtor == contextBuiltin.getOutput()) {
      return executionEnvironment.permissions.output();
    } else if (runtimeCtxCtor == contextBuiltin.getDataflowStackTrace()) {
      return executionEnvironment.permissions.dataflowStacktrace();
    } else {
      throw ensoCtx.raiseAssertionPanic(this, "Unknown context: " + runtimeCtxCtor, null);
    }
  }
}
