package org.enso;

import java.util.List;

public class ClassLoaderConstants {

  public static final List<String> RESOURCE_DELEGATION_PATTERNS = List.of("org.slf4j", "ch.qos");
  public static final List<String> CLASS_DELEGATION_PATTERNS =
      List.of("org.graalvm", "java", "org.slf4j", "ch.qos");
  static final String DEFAULT_RUNNER_JAR = "runner/runner.jar";
}
