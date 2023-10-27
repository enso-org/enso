package org.enso.interpreter.runtime;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Host class loader that serves as a replacement for {@link
 * com.oracle.truffle.host.HostClassLoader}. Add URLs to Jar archives with {@link #add(URL)}. All
 * the classes that are loded via this class loader are first searched inside those archives. If not
 * found, delegates to parent class loaders.
 */
public class HostClassLoader extends URLClassLoader {

  private final Map<String, Class<?>> loadedClasses = new ConcurrentHashMap<>();

  public HostClassLoader() {
    super(new URL[0]);
  }

  void add(URL u) {
    addURL(u);
  }

  @Override
  public Class<?> loadClass(String name) throws ClassNotFoundException {
    return loadClass(name, false);
  }

  @Override
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    var l = loadedClasses.get(name);
    if (l != null) {
      return l;
    }
    try {
      l = findClass(name);
      if (resolve) {
        l.getMethods();
      }
      loadedClasses.put(name, l);
      return l;
    } catch (ClassNotFoundException ex) {
      return super.loadClass(name, resolve);
    }
  }
}
