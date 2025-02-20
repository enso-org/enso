package org.enso.base;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/** A Java interface to the `Enso_Project` type. */
public final class CurrentEnsoProject {
  private final String name;
  private final String namespace;
  private final String rootPath;

  private static CurrentEnsoProject cached = null;
  private static boolean isCached = false;

  private CurrentEnsoProject(String name, String namespace, String rootPath) {
    this.name = name;
    this.namespace = namespace;
    this.rootPath = rootPath;
  }

  public static CurrentEnsoProject get() {
    if (!isCached) {
      Value ensoProject =
          EnsoMeta.callStaticModuleMethod("Standard.Base.Meta.Enso_Project", "enso_project");
      Value namespace = invokeMember("namespace", ensoProject);
      Value name = invokeMember("name", ensoProject);
      Value rootPath = invokeMember("path", invokeMember("root", ensoProject));
      if (namespace == null || name == null || rootPath == null) {
        cached = null;
      } else {
        cached = new CurrentEnsoProject(name.asString(), namespace.asString(), rootPath.asString());
      }

      isCached = true;
    }

    return cached;
  }

  private static Value invokeMember(String member, Value object, Object... args) {
    var meta = object.getMetaObject();
    if (meta.hasMember(member)) {
      return meta.invokeMember(member, object, args);
    } else {
      return null;
    }
  }

  public String getName() {
    return name;
  }

  public String getNamespace() {
    return namespace;
  }

  public String getRootPath() {
    return rootPath;
  }

  public String fullName() {
    return namespace + "." + name;
  }
}
