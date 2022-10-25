package org.enso.interpreter.runtime.state;

public record IOPermissions(String name, boolean isInputAllowed, boolean isOutputAllowed) {
  public static final IOPermissions PRODUCTION = new IOPermissions("production", true, true);
  public static final IOPermissions DEVELOPMENT = new IOPermissions("development", false, false);

  public static IOPermissions forName(String name) {
    switch (name) {
      case "production":
        return PRODUCTION;
      case "development":
        return DEVELOPMENT;
      default:
        return new IOPermissions(name, false, false);
    }
  }

  public IOPermissions allowInputIn(String name) {
    if (name.equals(name()) && !isInputAllowed()) {
      return new IOPermissions(name(), true, isOutputAllowed());
    }
    return this;
  }

  public IOPermissions allowOutputIn(String name) {
    if (name.equals(name()) && !isOutputAllowed()) {
      return new IOPermissions(name(), isInputAllowed(), true);
    }
    return this;
  }
}
