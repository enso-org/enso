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

  public static final ExecutionEnvironment LIVE = initLive();
  public static final ExecutionEnvironment DESIGN = new ExecutionEnvironment("design");

  private static final ExecutionEnvironment initLive() {
    Map<String, Boolean> contexts = new HashMap<>();
    contexts.put("Input", true);
    contexts.put("Output", true);
    return new ExecutionEnvironment("live", contexts);
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
      case "live":
        return LIVE;
      case "design":
        return DESIGN;
      default:
        return new ExecutionEnvironment(name);
    }
  }
}
