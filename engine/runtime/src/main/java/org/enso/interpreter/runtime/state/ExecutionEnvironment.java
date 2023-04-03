package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.runtime.Context;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;

import java.util.HashMap;
import java.util.Map;

public class ExecutionEnvironment {

  private final String name;
  private final Map<String, Boolean> permissions;

  public static final String LIVE_ENVIRONMENT_NAME = "live";

  public static final String DESIGN_ENVIRONMENT_NAME = "design";

  public static final ExecutionEnvironment LIVE = initLive(LIVE_ENVIRONMENT_NAME);
  public static final ExecutionEnvironment DESIGN =
      new ExecutionEnvironment(DESIGN_ENVIRONMENT_NAME);

  private static final ExecutionEnvironment initLive(String name) {
    Map<String, Boolean> contexts = new HashMap<>();
    contexts.put(Context.INPUT_CONTEXT, true);
    contexts.put(Context.OUTPUT_CONTEXT, true);
    return new ExecutionEnvironment(name, contexts);
  }

  public ExecutionEnvironment(String name) {
    this.name = name;
    this.permissions = new HashMap<>();
  }

  private ExecutionEnvironment(String name, Map<String, Boolean> permissions) {
    this.name = name;
    this.permissions = permissions;
  }

  public String getName() {
    return this.name;
  }

  @CompilerDirectives.TruffleBoundary
  public ExecutionEnvironment withContextEnabled(Atom context) {
    assert context.getType() == EnsoContext.get(null).getBuiltins().context().getType();
    HashMap<String, Boolean> permissions1 = new HashMap<>();
    for (Map.Entry<String, Boolean> permission : permissions.entrySet()) {
      permissions1.put(permission.getKey(), permission.getValue());
    }
    permissions1.put(context.getConstructor().getName(), true);
    return new ExecutionEnvironment(name, permissions1);
  }

  @CompilerDirectives.TruffleBoundary
  public ExecutionEnvironment withContextDisabled(Atom context) {
    assert context.getType() == EnsoContext.get(null).getBuiltins().context().getType();
    HashMap<String, Boolean> permissions1 = new HashMap<>();
    for (Map.Entry<String, Boolean> permission : permissions.entrySet()) {
      permissions1.put(permission.getKey(), permission.getValue());
    }
    permissions1.put(context.getConstructor().getName(), false);
    return new ExecutionEnvironment(name, permissions1);
  }

  @CompilerDirectives.TruffleBoundary
  public Boolean hasContextEnabled(String context) {
    return permissions.getOrDefault(context, false);
  }

  public static ExecutionEnvironment forName(String name) {
    switch (name) {
      case LIVE_ENVIRONMENT_NAME:
        return LIVE;
      case DESIGN_ENVIRONMENT_NAME:
        return DESIGN;
      default:
        throw new RuntimeException("Unsupported Execution Environment `" + name + "`");
    }
  }
}
