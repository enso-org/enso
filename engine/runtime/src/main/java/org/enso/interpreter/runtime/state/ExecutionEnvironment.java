package org.enso.interpreter.runtime.state;

import org.enso.interpreter.node.expression.builtin.runtime.Context;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;

public class ExecutionEnvironment {

  private final String name;

  // Ideally we would "just" use a map here. But that leads
  // to native image build problems. This in turn leads to
  // TruffleBoundary annotations which in turn leads to slow path.
  private final String[] keys;
  private final Boolean[] permissions;

  public static final String LIVE_ENVIRONMENT_NAME = "live";

  public static final String DESIGN_ENVIRONMENT_NAME = "design";

  public static final ExecutionEnvironment LIVE = initLive(LIVE_ENVIRONMENT_NAME);
  public static final ExecutionEnvironment DESIGN =
      new ExecutionEnvironment(DESIGN_ENVIRONMENT_NAME);

  private static final ExecutionEnvironment initLive(String name) {
    String[] keys = new String[] {Context.INPUT_NAME, Context.OUTPUT_NAME};
    Boolean[] permissions = new Boolean[] {true, true};
    return new ExecutionEnvironment(name, keys, permissions);
  }

  public ExecutionEnvironment(String name) {
    this.name = name;
    this.keys = new String[0];
    this.permissions = new Boolean[0];
  }

  private ExecutionEnvironment(String name, String[] keys, Boolean[] permissions) {
    this.name = name;
    this.keys = keys;
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
    assert context.getType() == EnsoContext.get(null).getBuiltins().context().getType();
    int keyFound = -1;
    for (int i = 0; i < keys.length; i++) {
      if (keys[i].equals(context.getConstructor().getName())) {
        keyFound = i;
      }
    }
    String[] keys1;
    Boolean[] permissions1;
    if (keyFound != -1) {
      keys1 = cloneArray(keys, new String[keys.length]);
      permissions1 = cloneArray(permissions, new Boolean[keys.length]);
      permissions1[keyFound] = value;
    } else {
      keys1 = cloneArray(keys, new String[keys.length + 1]);
      permissions1 = cloneArray(permissions, new Boolean[keys.length + 1]);
      keyFound = keys.length;
      keys1[keyFound] = context.getConstructor().getName();
      permissions1[keyFound] = value;
    }
    return new ExecutionEnvironment(name, keys1, permissions1);
  }

  private <T> T[] cloneArray(T[] fromArray, T[] toArray) {
    for (int i = 0; i < fromArray.length; i++) {
      toArray[i] = fromArray[i];
    }
    return toArray;
  }

  public Boolean hasContextEnabled(String context) {
    int keyFound = -1;
    for (int i = 0; i < keys.length; i++) {
      if (keys[i].equals(context)) {
        keyFound = i;
      }
    }
    if (keyFound != -1) {
      return permissions[keyFound];
    } else {
      return false;
    }
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
