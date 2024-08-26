package org.enso.base;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/** A Java interface to the `Enso_Project` type. */
public final class CurrentEnsoProject {
  private final String name;
  private final String namespace;

  private static CurrentEnsoProject cached = null;
  private static boolean isCached = false;

  private CurrentEnsoProject(String name, String namespace) {
    this.name = name;
    this.namespace = namespace;
  }

  public static CurrentEnsoProject get() {
    if (!isCached) {
      Value ensoProject = EnsoMeta.callStaticModuleMethod("Standard.Base.Meta.Enso_Project", "enso_project");
      if (ensoProject.hasMember("name") && ensoProject.hasMember("namespace")) {
        Value namespace = ensoProject.invokeMember("namespace");
        Value name = ensoProject.invokeMember("name");
        if (namespace == null || name == null) {
          cached = null;
        } else {
          cached = new CurrentEnsoProject(name.asString(), namespace.asString());
        }
      } else {
        cached = null;
      }

      isCached = true;
    }

    return cached;
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
