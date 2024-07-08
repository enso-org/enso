package org.enso.projectmanager.infrastructure;

import org.enso.projectmanager.service.versionmanagement.NoOpInterface;
import org.enso.projectmanager.versionmanagement.DefaultDistributionConfiguration;
import org.enso.runtimeversionmanager.components.RuntimeVersionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class PropertiesSetup {

  private static final Logger logger = LoggerFactory.getLogger(PropertiesSetup.class);

  private static final RuntimeVersionManager runtimeVersionManager =
      DefaultDistributionConfiguration.makeRuntimeVersionManager(new NoOpInterface());

  private PropertiesSetup() {}

  /**
   * Setup {@code java.home} system property.
   *
   * <p>{@code java.home} system property is required for Java AWT to work in the native image.
   *
   * @see <a href="https://github.com/oracle/graal/issues/3659">graal/3659</a>.
   */
  public static void setupJavaHome() {
    var propJavaHome = System.getProperty("java.home");
    logger.debug("java.home={}", propJavaHome);

    if (System.getProperty("java.home") == null) {
      var envJavaHome = System.getenv("JAVA_HOME");
      logger.debug("JAVA_HOME={}", envJavaHome);
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
