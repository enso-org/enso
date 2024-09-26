package org.enso.interpreter.runtime;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.graalvm.polyglot.Context;
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
  // Classes from "org.graalvm" packages are loaded either by a class loader for the boot
  // module layer, or by a specific class loader, depending on how enso is run. For example,
  // if enso is run via `org.graalvm.polyglot.Context.eval` from `javac`, then the graalvm
  // classes are loaded via a class loader somehow created by `javac` and not by the boot
  // module layer's class loader.
  private static final ClassLoader polyglotClassLoader = Context.class.getClassLoader();

  // polyglotClassLoader will be used only iff `org.enso.runtime` module is not in the
  // boot module layer.
  private static final boolean isRuntimeModInBootLayer;

  public HostClassLoader() {
    super(new URL[0]);
  }

  static {
    var bootModules = ModuleLayer.boot().modules();
    var hasRuntimeMod =
        bootModules.stream().anyMatch(module -> module.getName().equals("org.enso.runtime"));
    isRuntimeModInBootLayer = hasRuntimeMod;
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
    if (!isRuntimeModInBootLayer && name.startsWith("org.graalvm")) {
      return polyglotClassLoader.loadClass(name);
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
