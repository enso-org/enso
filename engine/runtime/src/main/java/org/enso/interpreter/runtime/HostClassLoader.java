package org.enso.interpreter.runtime;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Host class loader that serves as a replacement for {@link
 * com.oracle.truffle.host.HostClassLoader}. Add URLs to Jar archives with {@link #add(URL)}. All
 * the classes that are loaded via this class loader are first searched inside those archives. If
 * not found, delegates to parent class loaders.
 */
final class HostClassLoader extends URLClassLoader implements AutoCloseable {

  private final Map<String, Class<?>> loadedClasses = new ConcurrentHashMap<>();
  private static final Logger logger = LoggerFactory.getLogger(HostClassLoader.class);

  public HostClassLoader() {
    super(new URL[0]);
  }

  void add(URL u) {
    logger.debug("Adding URL '{}' to class path", u);
    addURL(u);
  }

  @Override
  public Class<?> loadClass(String name) throws ClassNotFoundException {
    return loadClass(name, false);
  }

  @Override
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    logger.trace("Loading class {}", name);
    var l = loadedClasses.get(name);
    if (l != null) {
      logger.trace("Class {} found in cache", name);
      return l;
    }
    try {
      l = findClass(name);
      if (resolve) {
        l.getMethods();
      }
      logger.trace("Class {} found, putting in cache", name);
      loadedClasses.put(name, l);
      return l;
    } catch (ClassNotFoundException ex) {
      logger.trace("Class {} not found, delegating to super", name);
      return super.loadClass(name, resolve);
    }
  }

  @Override
  public void close() {
    loadedClasses.clear();
  }
}
