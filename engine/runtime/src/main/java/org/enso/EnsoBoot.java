package org.enso;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

public final class EnsoBoot {
  private EnsoBoot() {}

  public static void main(String[] args) throws Exception {
    var jar = new File("runner.jar");
    if (!jar.exists()) {
      throw new IllegalStateException("Can find " + jar);
    }
    var url = jar.toURI().toURL();

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
