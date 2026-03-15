package org.enso.jvm.interop.impl;

import java.io.File;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Set;

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
    var dumpMessagesLimit = System.getProperty(OtherJvmPool.DUMP_MESSAGE_PROPERTY);
    if (dumpMessagesLimit != null) {
      commandAndArgs.add("-D" + OtherJvmPool.DUMP_MESSAGE_PROPERTY + "=" + dumpMessagesLimit);
    }
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
      var components = ModuleFinder.of(component.toPath());
      var finder = ModuleFinder.compose(components, ModuleFinder.ofSystem());
      var paths =
          moduleNamesOrNull.stream()
              .map(
                  (n) -> {
                    var opt = finder.find(n);
                    if (opt.isEmpty()) {
                      throw new IllegalStateException(
                          "Cannot find module " + n + " at " + component + " result: " + opt);
                    }
                    commandAndArgs.add("--add-modules=" + n);
                    return opt.get().location().orElse(null);
                  })
              .filter(u -> u != null && "file".equals(u.getScheme()))
              .map(File::new)
              .map(File::getPath);
      modulePath = String.join(File.pathSeparator, paths.toArray(String[]::new));
    }
    commandAndArgs.add("--module-path=" + modulePath);
    commandAndArgs.add("-Djdk.module.main=" + mainModule);
    // commandAndArgs.add("-Djdk.module.showModuleResolution=true");
    commandAndArgs.add(
        "-Dslf4j.provider=org.enso.logging.config.systemlogger.Slf4jViaSystemProvider");
    commandAndArgs.add("-Dslf4j.internal.verbosity=WARN");
    return commandAndArgs.toArray(String[]::new);
  }
}
