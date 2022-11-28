package org.enso.interpreter.runtime.state;

public class IOPermissions {
  private final String name;
  private final boolean isInputAllowed;
  private final boolean isOutputAllowed;

  private IOPermissions(String name, boolean isInputAllowed, boolean isOutputAllowed) {
    this.name = name;
    this.isInputAllowed = isInputAllowed;
    this.isOutputAllowed = isOutputAllowed;
  }

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
    if (name.equals(this.name) && !isInputAllowed) {
      return new IOPermissions(this.name, true, isOutputAllowed);
    }
    return this;
  }

  public IOPermissions allowOutputIn(String name) {
    if (name.equals(this.name) && !isOutputAllowed) {
      return new IOPermissions(this.name, isInputAllowed, true);
    }
    return this;
  }

  public boolean isInputAllowed() {
    return isInputAllowed;
  }

  public boolean isOutputAllowed() {
    return isOutputAllowed;
  }
}
