package org.enso;

import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;

/**
 * {@code runner.jar} is a fat jar containing all the dependencies for engine-runner, however, it
 * cannot be put on module-path, neither class-path, because it cannot be used in conjunction with
 * {@code runtime.jar} fat jar. For now, this class is a workaround that just tries to invoke {@link
 * org.enso.runner.Main.main} from {@code runner.jar} using a custom class loader.
 */
public final class EngineRunnerBootLoader {
  private EngineRunnerBootLoader() {}

  private static final String defaultRunnerJar = "runner/runner.jar";

  public static void main(String[] args) throws Exception {
    var ensoRunnerProp = System.getProperty("enso.runner");
    var runnerJarPath =
        ensoRunnerProp != null ? Path.of(ensoRunnerProp) : getDefaultRunnerJarPath();
    if (!runnerJarPath.toFile().exists()) {
      throw new IllegalStateException("Cannot find runner fat jar at " + runnerJarPath);
    }
    var url = runnerJarPath.toUri().toURL();
    var parentLoader = ClassLoader.getSystemClassLoader();

    try (var loader = new PrefferingLoader(new URL[] {url}, parentLoader)) {
      var clazz = loader.loadClass("org.enso.runner.Main");
      var main = clazz.getMethod("main", String[].class);
      main.invoke(null, (Object) args);
    }
  }

  private static Path getDefaultRunnerJarPath() {
    var runtimeJar =
        EngineRunnerBootLoader.class.getProtectionDomain().getCodeSource().getLocation();
    try {
      var runnerJarUri = runtimeJar.toURI().resolve(defaultRunnerJar);
      return Path.of(runnerJarUri);
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }

  private static class PrefferingLoader extends URLClassLoader {
    PrefferingLoader(URL[] urls, ClassLoader parentLoader) {
      super(urls, parentLoader);
    }

    @Override
    public Class<?> loadClass(String name) throws ClassNotFoundException {
      try {
        return findClass(name);
      } catch (ClassNotFoundException ex) {
        return super.loadClass(name);
      }
    }

    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
      try {
        var clazz = findClass(name);
        if (resolve) {
          clazz.getMethods();
        }
        return clazz;
      } catch (ClassNotFoundException ex) {
        return super.loadClass(name, resolve);
      }
    }
  }
}
