package org.enso.version;

/**
 * Class containing only static methods for getting various version information gathered at build
 * time.
 */
public class BuildVersion {
  private BuildVersion() {}

  public static String defaultDevEnsoVersion() {
    return GeneratedVersion.defaultDevEnsoVersion();
  }

  public static String ensoVersion() {
    return GeneratedVersion.ensoVersion();
  }

  public static String scalacVersion() {
    return GeneratedVersion.scalacVersion();
  }

  /**
   * Version of GraalVM, more specifically, version of the GraalVM and Truffle libraries used to
   * build the engine.
   */
  public static String graalVersion() {
    return GeneratedVersion.graalVersion();
  }

  /**
   * Version of Java (JDK) used to build the engine.
   *
   * @return
   */
  public static String javaVersion() {
    return GeneratedVersion.javaVersion();
  }

  public static String currentEdition() {
    return GeneratedVersion.currentEdition();
  }

  /** Current commit ID. */
  public static String commit() {
    return GeneratedVersion.commit();
  }

  /** Current git ref */
  public static String ref() {
    return GeneratedVersion.ref();
  }

  public static boolean isDirty() {
    return GeneratedVersion.isDirty();
  }

  public static String latestCommitDate() {
    return GeneratedVersion.latestCommitDate();
  }

  /**
   * Release mode, set to true if the environment variable {@code ENSO_RELEASE_MODE} is set to
   * {@code true} at build time.
   */
  public static boolean isRelease() {
    return GeneratedVersion.isRelease();
  }
}
