package org.enso.interpreter.runtime.state;

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
