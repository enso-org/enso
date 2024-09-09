package org.enso.interpreter.bench.result;

/**
 * @See `results_schema.json` for the schema of the results file.
 */
public record Configuration(
    String osName,
    String osArch,
    String osVersion,
    String vmName,
    String vmVersion,
    String vmVendor,
    String jdkVersion) {
  public static Configuration fromSystemProperties() {
    return new Configuration(
        System.getProperty("os.name"),
        System.getProperty("os.arch"),
        System.getProperty("os.version"),
        System.getProperty("java.vm.name"),
        System.getProperty("java.vm.version"),
        System.getProperty("java.vm.vendor"),
        System.getProperty("java.version"));
  }
}
