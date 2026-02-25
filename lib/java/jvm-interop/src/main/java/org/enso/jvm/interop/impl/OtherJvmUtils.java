package org.enso.jvm.interop.impl;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Stream;

public final class OtherJvmUtils {
  private OtherJvmUtils() {}

  /**
   * Finds directory where the Enso modules are stored.
   *
   * @return directory where all the modules are stored
   */
  public static File findModules() {
    try {
      var loc = OtherJvmUtils.class.getProtectionDomain().getCodeSource().getLocation();
      var component = new File(loc.toURI().resolve("..")).getAbsoluteFile();
      if (!component.getName().equals("component")) {
        component = new File(component, "component");
      }
      return component;
    } catch (URISyntaxException ex) {
      throw new IllegalStateException(ex);
    }
  }

  public static String[] findJvmArgs(
      File javaHome, String mainModule, Set<String> moduleNamesOrNull) throws IOException {
    var component = findModules();
    if (!javaHome.exists()) {
      throw new IOException("JVM doesn't exists: " + javaHome);
    }
    var commandAndArgs = new ArrayList<String>();
    var assertsOn = false;
    assert assertsOn = true;
    if (assertsOn) {
      commandAndArgs.add("-ea");
    }
    commandAndArgs.add("--sun-misc-unsafe-memory-access=allow");
    commandAndArgs.add("-Dpolyglot.engine.WarnInterpreterOnly=false");
    commandAndArgs.add("-Dtruffle.UseFallbackRuntime=true");
    commandAndArgs.add("--enable-native-access=org.graalvm.truffle");
    commandAndArgs.add("--enable-native-access=org.enso.jvm.channel");
    commandAndArgs.add("--add-opens=java.base/java.nio=ALL-UNNAMED");
    if (!component.isDirectory()) {
      throw new IOException("Cannot find " + component + " directory");
    }
    String modulePath;
    if (moduleNamesOrNull == null) {
      modulePath = component.getPath();
    } else {
      var moduleNames =
          Set.of(
              "jvm-interop.jar",
              "logging-system2slf4j.jar",
              "polyglot-25.0.1.jar",
              "jvm-channel.jar",
              "persistance.jar",
              "slf4j-api-2.0.16.jar",
              "truffle-api-25.0.1.jar",
              "engine-common.jar");
      var files = component.listFiles((n) -> moduleNames.contains(n.getName()));
      assert files.length == moduleNames.size() : "Found all names: " + Arrays.toString(files);
      var paths = Stream.of(files).map(File::getPath);
      modulePath = String.join(File.pathSeparator, paths.toArray(String[]::new));
    }
    commandAndArgs.add("--module-path=" + modulePath);
    commandAndArgs.add("-Djdk.module.main=" + mainModule);
    // commandAndArgs.add("-Dslf4j.provider=org.enso.jvm.interop.impl.OtherJvmLogger");
    commandAndArgs.add("-Djdk.module.showModuleResolution=true");
    return commandAndArgs.toArray(String[]::new);
  }
}
