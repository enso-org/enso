package org.enso;

import java.util.List;

public class ClassLoaderConstants {

  /**
   * Prefix of names of classes that are know to be on the system module-path, i.e., on the boot
   * module layer. Engine is started from {@link EngineRunnerBootLoader} with a custom class loader
   * that is isolated form the system's module-path. Delegating to either system class loader, or
   * any other class loader that has access to the boot module layer, ensures that the Truffle
   * polyglot environment is properly initialized and also that the global logging configuration is
   * consistent.
   */
  public static final List<String> CLASS_DELEGATION_PATTERNS =
      List.of("org.graalvm", "java", "org.slf4j", "ch.qos");

  public static final List<String> RESOURCE_DELEGATION_PATTERNS = List.of("org.slf4j", "ch.qos");
  /**
   * Path to the {@code runner.jar} fat jar. This must not be on the system's module-path, because
   * the JVM would not be able to boot.
   */
  static final String DEFAULT_RUNNER_JAR = "runner/runner.jar";
}
