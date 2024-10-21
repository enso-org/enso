package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;

/**
 * A node representing functionality done by {@code Standard.Base.Runtime.Context.with_enabled} and
 * {@code Standard.Base.Runtime.Context.with_disabled}. That is, it enables or disables the given
 * context in the current {@link ExecutionEnvironment execution environment}.
 */
@GenerateUncached
public abstract class WithContextNode extends Node {
  public static WithContextNode getUncached() {
    return WithContextNodeGen.getUncached();
  }

  public static WithContextNode create() {
    return WithContextNodeGen.create();
  }

  /**
   * Returns a new {@link ExecutionEnvironment} with the given context enabled or disabled.
   *
   * @param current Current execution environment.
   * @param context Atom of type {@code Standard.Base.Runtime.Context}.
   * @param enabled Whether to enable or disable the context.
   */
  public abstract ExecutionEnvironment executeEnvironmentUpdate(
      ExecutionEnvironment current, Atom context, boolean enabled);

  @Specialization
  ExecutionEnvironment doIt(ExecutionEnvironment current, Atom context, boolean enabled) {
    var ensoCtx = EnsoContext.get(this);
    var contextBuiltin = ensoCtx.getBuiltins().context();
    if (context.getConstructor().getType() != contextBuiltin.getType()) {
      throw ensoCtx.raiseAssertionPanic(this, "Invalid context type", null);
    }
    var ctor = context.getConstructor();
    ContextPermissions newPermissions;
    if (ctor == contextBuiltin.getInput()) {
      newPermissions =
          new ContextPermissions(
              enabled, current.permissions.output(), current.permissions.dataflowStacktrace());
    } else if (ctor == contextBuiltin.getOutput()) {
      newPermissions =
          new ContextPermissions(
              current.permissions.input(), enabled, current.permissions.dataflowStacktrace());
    } else if (ctor == contextBuiltin.getDataflowStackTrace()) {
      newPermissions =
          new ContextPermissions(
              current.permissions.input(), current.permissions.output(), enabled);
    } else {
      throw ensoCtx.raiseAssertionPanic(this, "Unknown context: " + ctor, null);
    }
    return new ExecutionEnvironment(current.getName(), newPermissions);
  }
}
