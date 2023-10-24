package org.enso;

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;

public final class EnsoBoot {
  private EnsoBoot() {}

  public static void main(String[] args) throws Exception {
    var runtimeJar = EnsoBoot.class.getProtectionDomain().getCodeSource().getLocation();
    var runnerJarUri = runtimeJar.toURI().resolve("runner/runner.jar");
    var runnerJarPath = Path.of(runnerJarUri);
    var runnerJar = runnerJarPath.toFile();
    if (!runnerJar.exists()) {
      throw new IllegalStateException("Cannot find " + runnerJar + ". Tried path: " + runnerJarPath);
    }
    var url = runnerJar.toURI().toURL();

    var loader = new PrefferingLoader(new URL[] {url});
    var clazz = loader.loadClass("org.enso.runner.Main");
    var main = clazz.getMethod("main", String[].class);
    main.invoke(null, (Object) args);
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
