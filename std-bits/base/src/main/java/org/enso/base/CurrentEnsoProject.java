package org.enso.base;

/** A Java interface to the `Enso_Project` type. */
public final class CurrentEnsoProject {
  private final String name;
  private final String namespace;

  private CurrentEnsoProject(String name, String namespace) {
    this.name = name;
    this.namespace = namespace;
  }

  public static CurrentEnsoProject get() {
    // TODO this currently does not work, because of bug
    // https://github.com/enso-org/enso/issues/9845
    return null;
  }

  public String getName() {
    return name;
  }

  public String getNamespace() {
    return namespace;
  }

  public String fullName() {
    return namespace + "." + name;
  }
}
