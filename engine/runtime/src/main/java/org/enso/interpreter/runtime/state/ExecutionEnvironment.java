package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

public class ExecutionEnvironment {

  private final String name;

  private final Permissions permissions;

  public static final String LIVE_ENVIRONMENT_NAME = "live";

  public static final String DESIGN_ENVIRONMENT_NAME = "design";

  public static final ExecutionEnvironment LIVE = initLive(LIVE_ENVIRONMENT_NAME);
  public static final ExecutionEnvironment DESIGN =
      new ExecutionEnvironment(DESIGN_ENVIRONMENT_NAME);

  private static ExecutionEnvironment initLive(String name) {
    var permissions = new Permissions(true, true, false);
    return new ExecutionEnvironment(name, permissions);
  }

  public ExecutionEnvironment(String name) {
    this.name = name;
    this.permissions = new Permissions(false, false, false);
  }

  private ExecutionEnvironment(String name, Permissions permissions) {
    this.name = name;
    this.permissions = permissions;
  }

  public String getName() {
    return this.name;
  }

  public ExecutionEnvironment withContextEnabled(Atom context) {
    return update(context, true);
  }

  public ExecutionEnvironment withContextDisabled(Atom context) {
    return update(context, false);
  }

  private ExecutionEnvironment update(Atom context, boolean value) {
    var contextBuiltin = EnsoContext.get(null).getBuiltins().context();
    assert context.getConstructor().getType() == contextBuiltin.getType();
    var ctor = context.getConstructor();
    Permissions newPermissions;
    if (ctor == contextBuiltin.getInput()) {
      newPermissions = new Permissions(value, permissions.output, permissions.dataflowStacktrace);
    } else if (ctor == contextBuiltin.getOutput()) {
      newPermissions = new Permissions(permissions.input, value, permissions.dataflowStacktrace);
    } else if (ctor == contextBuiltin.getDataflowStackTrace()) {
      newPermissions = new Permissions(permissions.input, permissions.output, value);
    } else {
      throw CompilerDirectives.shouldNotReachHere("Unknown context `" + ctor.getName() + "`");
    }
    return new ExecutionEnvironment(name, newPermissions);
  }

  /**
   * Checks if the context is enabled in this execution environment.
   *
   * @param runtimeCtx Constructor of {@code Standard.Base.Runtime.Context} builtin type.
   * @return {@code true} if the context is enabled in this execution environment.
   */
  public boolean hasContextEnabled(AtomConstructor runtimeCtx, EnsoContext ensoCtx) {
    var contextBuiltin = ensoCtx.getBuiltins().context();
    if (runtimeCtx == contextBuiltin.getInput()) {
      return permissions.input;
    } else if (runtimeCtx == contextBuiltin.getOutput()) {
      return permissions.output;
    } else if (runtimeCtx == contextBuiltin.getDataflowStackTrace()) {
      return permissions.dataflowStacktrace;
    }
    throw CompilerDirectives.shouldNotReachHere(
        "Unknown runtimeCtx `" + runtimeCtx.getName() + "`");
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

  /**
   * Fields correspond to the constructors of {@code Standard.Base.Runtime.Context} builtin type.
   */
  private record Permissions(boolean input, boolean output, boolean dataflowStacktrace) {}
}
