package org.enso.interpreter.runtime;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.pkg.NativeLibraryFinder;
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
    if (name.startsWith("org.slf4j")) {
      // Delegating to system class loader ensures that log classes are not loaded again
      // and do not require special setup. In other words, it is using log configuration that
      // has been setup by the runner that started the process. See #11641.
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

  /**
   * Find the library with the specified name inside the {@code polyglot/lib} directory of caller's
   * project. The search inside the {@code polyglot/lib} directory hierarchy is specified by <a
   * href="https://bits.netbeans.org/23/javadoc/org-openide-modules/org/openide/modules/doc-files/api.html#jni">NetBeans
   * JNI specification</a>.
   *
   * <p>Note: The current implementation iterates all the {@code polyglot/lib} directories of all
   * the packages.
   *
   * @param libname The library name. Without platform-specific suffix or prefix.
   * @return Absolute path to the library if found, or null.
   */
  @Override
  protected String findLibrary(String libname) {
    var pkgRepo = EnsoContext.get(null).getPackageRepository();
    for (var pkg : pkgRepo.getLoadedPackagesJava()) {
      var libPath = NativeLibraryFinder.findNativeLibrary(libname, pkg, TruffleFileSystem.INSTANCE);
      if (libPath != null) {
        return libPath;
      }
    }
    logger.trace("Native library {} not found in any package", libname);
    return null;
  }

  @Override
  public void close() {
    loadedClasses.clear();
  }
}
