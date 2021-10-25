package org.enso.interpreter.runtime.util;

/** A representation of a package that has been shadowed by another package of the same name. */
public class ShadowedPackage {
  private final String shadowedPath;
  private final String shadowingPath;
  private final String packageName;

  /**
   * Constructs a new representation of a shadowed package.
   *
   * @param shadowedPath the path of the package being shadowed
   * @param shadowingPath the path of the package shadowing {@code shadowedPath}
   * @param packageName the package name that has clashed
   */
  public ShadowedPackage(String shadowedPath, String shadowingPath, String packageName) {
    this.shadowedPath = shadowedPath;
    this.shadowingPath = shadowingPath;
    this.packageName = packageName;
  }

  /** @return the path to the shadowed package */
  public String getShadowedPath() {
    return shadowedPath;
  }

  /** @return the path to the package being shadowed */
  public String getShadowingPath() {
    return shadowingPath;
  }

  /** @return the package name that has clashed */
  public String getPackageName() {
    return packageName;
  }

  @Override
  public String toString() {
    return "The package "
        + packageName
        + " at path `"
        + shadowedPath
        + "` is shadowed by a package of the same name at path `"
        + shadowingPath
        + "`.";
  }
}
