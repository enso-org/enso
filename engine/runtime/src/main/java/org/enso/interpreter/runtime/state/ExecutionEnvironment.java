package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.data.text.Text;

public final class ExecutionEnvironment {
  private final Text name;

  final ContextPermissions permissions;

  private static final String LIVE_ENVIRONMENT_NAME = "live";
  private static final String DESIGN_ENVIRONMENT_NAME = "design";

  public static final ExecutionEnvironment LIVE;

  static {
    var perm = new ContextPermissions(true, true, false);
    LIVE = new ExecutionEnvironment(Text.create(LIVE_ENVIRONMENT_NAME), perm);
  }

  public static final ExecutionEnvironment DESIGN;

  static {
    var perm = new ContextPermissions(false, false, false);
    DESIGN = new ExecutionEnvironment(Text.create(DESIGN_ENVIRONMENT_NAME), perm);
  }

  private ExecutionEnvironment(Text name, ContextPermissions permissions) {
    this.name = name;
    this.permissions = permissions;
  }

  @CompilerDirectives.TruffleBoundary
  public String getName() {
    return this.name.toString();
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

  ExecutionEnvironment withPermissions(ContextPermissions permissions) {
    var derivedName = Text.create(name, "+");
    return new ExecutionEnvironment(derivedName, permissions);
  }
}
