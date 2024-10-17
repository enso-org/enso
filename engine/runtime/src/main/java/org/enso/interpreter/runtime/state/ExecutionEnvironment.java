package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.runtime.Context;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

public class ExecutionEnvironment {

  private final String name;

  final ContextPermissions permissions;

  public static final String LIVE_ENVIRONMENT_NAME = "live";

  public static final String DESIGN_ENVIRONMENT_NAME = "design";

  public static final ExecutionEnvironment LIVE = initLive(LIVE_ENVIRONMENT_NAME);
  public static final ExecutionEnvironment DESIGN =
      new ExecutionEnvironment(DESIGN_ENVIRONMENT_NAME);

  private static ExecutionEnvironment initLive(String name) {
    var permissions = new ContextPermissions(true, true, false);
    return new ExecutionEnvironment(name, permissions);
  }

  public ExecutionEnvironment(String name) {
    this.name = name;
    this.permissions = new ContextPermissions(false, false, false);
  }

  ExecutionEnvironment(String name, ContextPermissions permissions) {
    this.name = name;
    this.permissions = permissions;
  }

  public String getName() {
    return this.name;
  }

  /**
   * Returns copy of this {@link ExecutionEnvironment} with new permissions for the given context.
   *
   * @param ctor Constructor of {@code Standard.Base.Runtime.Context} type.
   * @param contextBuiltin The builtin type of {@code Standard.Base.Runtime.Context}.
   * @param value The new value for the permissions.
   * @return A copy of the execution environment with the new permissions.
   */
  ExecutionEnvironment update(AtomConstructor ctor, Context contextBuiltin, boolean value) {
    assert ctor.getType() == contextBuiltin.getType();
    ContextPermissions newPermissions;
    if (ctor == contextBuiltin.getInput()) {
      newPermissions =
          new ContextPermissions(value, permissions.output(), permissions.dataflowStacktrace());
    } else if (ctor == contextBuiltin.getOutput()) {
      newPermissions =
          new ContextPermissions(permissions.input(), value, permissions.dataflowStacktrace());
    } else if (ctor == contextBuiltin.getDataflowStackTrace()) {
      newPermissions = new ContextPermissions(permissions.input(), permissions.output(), value);
    } else {
      throw CompilerDirectives.shouldNotReachHere("Unknown context `" + ctor.getName() + "`");
    }
    return new ExecutionEnvironment(name, newPermissions);
  }

  public static ExecutionEnvironment forName(String name) {
    switch (name) {
      case LIVE_ENVIRONMENT_NAME:
        return LIVE;
      case DESIGN_ENVIRONMENT_NAME:
        return DESIGN;
      default:
        throw new IllegalArgumentException("Unsupported Execution Environment `" + name + "`");
    }
  }
}
