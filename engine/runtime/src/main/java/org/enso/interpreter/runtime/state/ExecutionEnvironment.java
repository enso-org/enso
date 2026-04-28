package org.enso.interpreter.runtime.state;

public final class ExecutionEnvironment {
  private final String name;

  final ContextPermissions permissions;

  private static final String LIVE_ENVIRONMENT_NAME = "live";
  private static final String DESIGN_ENVIRONMENT_NAME = "design";

  public static final ExecutionEnvironment LIVE;

  static {
    var perm = new ContextPermissions(true, true, false);
    LIVE = new ExecutionEnvironment(LIVE_ENVIRONMENT_NAME, perm);
  }

  public static final ExecutionEnvironment DESIGN;

  static {
    var perm = new ContextPermissions(false, false, false);
    DESIGN = new ExecutionEnvironment(DESIGN_ENVIRONMENT_NAME, perm);
  }

  private ExecutionEnvironment(String name, ContextPermissions permissions) {
    this.name = name;
    this.permissions = permissions;
  }

  public String getName() {
    return this.name;
  }

  public static ExecutionEnvironment forName(String name) {
    return switch (name) {
      case LIVE_ENVIRONMENT_NAME -> LIVE;
      case DESIGN_ENVIRONMENT_NAME -> DESIGN;
      default ->
          throw new IllegalArgumentException("Unsupported Execution Environment `" + name + "`");
    };
  }

  @Override
  public String toString() {
    return "ExecutionEnvironment[name=" + name + ", permissions=" + permissions + "]";
  }

  public ExecutionEnvironment withPermissions(ContextPermissions permissions) {
    var derivedName = getName() + "+";
    return new ExecutionEnvironment(derivedName, permissions);
  }
}
