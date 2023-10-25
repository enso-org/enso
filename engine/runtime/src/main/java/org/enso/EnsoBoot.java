package org.enso;

import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;

public final class EnsoBoot {
  private EnsoBoot() {}

  private static final String defaultRunnerJar = "runner/runner.jar";

  public static void main(String[] args) throws Exception {
    var ensoRunnerProp = System.getProperty("enso.runner");
    var runnerJarPath = ensoRunnerProp != null ? Path.of(ensoRunnerProp) : getDefaultRunnerJarPath();
    if (!runnerJarPath.toFile().exists()) {
      throw new IllegalStateException("Cannot find runner fat jar at " + runnerJarPath);
    }
    var url = runnerJarPath.toUri().toURL();

    var loader = new PrefferingLoader(new URL[] {url});
    var clazz = loader.loadClass("org.enso.runner.Main");
    var main = clazz.getMethod("main", String[].class);
    main.invoke(null, (Object) args);
  }

  private static Path getDefaultRunnerJarPath() {
    var runtimeJar = EnsoBoot.class.getProtectionDomain().getCodeSource().getLocation();
    try {
      var runnerJarUri = runtimeJar.toURI().resolve(defaultRunnerJar);
      return Path.of(runnerJarUri);
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }

  private static class PrefferingLoader extends URLClassLoader {
    PrefferingLoader(URL[] urls) {
      super(urls);
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
