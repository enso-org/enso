package org.enso.projectmanager.infrastructure;

import org.enso.runtimeversionmanager.components.RuntimeVersionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class PropertiesSetup {

  private static final Logger logger = LoggerFactory.getLogger(PropertiesSetup.class);

  private PropertiesSetup() {}

  /**
   * Setup {@code java.home} system property.
   *
   * <p>{@code java.home} system property is required for Java Abstract Window Toolkit to work in
   * the native image.
   *
   * @see <a href="https://github.com/oracle/graal/issues/3659">graal/3659</a>.
   */
  public static void setupJavaHome(RuntimeVersionManager runtimeVersionManager) {
    var propJavaHome = System.getProperty("java.home");
    logger.debug("System property java.home={}", propJavaHome);

    if (propJavaHome == null) {
      var envJavaHome = System.getenv("JAVA_HOME");
      logger.debug("Environment variable JAVA_HOME={}", envJavaHome);
      if (envJavaHome != null) {
        setProperty("java.home", envJavaHome);
        return;
      }

      var graalRuntimeOption = runtimeVersionManager.listInstalledGraalRuntimes();
      if (graalRuntimeOption.isEmpty()) {
        logger.error("Failed to locate GraalVM runtime.");
        return;
      }

      var graalRuntime = graalRuntimeOption.head();
      var javaHome = graalRuntime.javaHome();
      setProperty("java.home", javaHome.toString());
    }
  }

  private static void setProperty(String key, String value) {
    logger.debug("Setting system property {}={}", key, value);
    System.setProperty(key, value);
  }
}
