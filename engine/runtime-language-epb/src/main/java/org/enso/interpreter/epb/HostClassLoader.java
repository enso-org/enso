package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.File;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.graalvm.polyglot.Context;

/**
 * Host class loader that serves as a replacement for {@link
 * com.oracle.truffle.host.HostClassLoader}. All the classes that are loaded via this class loader
 * are first searched inside those archives. If not found, delegates to parent class loaders.
 */
@ExportLibrary(InteropLibrary.class)
final class HostClassLoader extends URLClassLoader implements AutoCloseable, TruffleObject {

  private final Map<String, Class<?>> loadedClasses = new ConcurrentHashMap<>();
  private static final Logger logger = System.getLogger(HostClassLoader.class.getName());
  // Classes from "org.graalvm" packages are loaded either by a class loader for the boot
  // module layer, or by a specific class loader, depending on how enso is run. For example,
  // if enso is run via `org.graalvm.polyglot.Context.eval` from `javac`, then the graalvm
  // classes are loaded via a class loader somehow created by `javac` and not by the boot
  // module layer's class loader.
  private static final ClassLoader polyglotClassLoader = Context.class.getClassLoader();

  // polyglotClassLoader will be used only iff `org.enso.runtime` module is not in the
  // boot module layer.
  private static final boolean isRuntimeModInBootLayer;
  private Object findLibraries;

  public HostClassLoader() {
    super(new URL[0]);
  }

  static {
    var bootModules = ModuleLayer.boot().modules();
    var hasRuntimeMod =
        bootModules.stream().anyMatch(module -> module.getName().equals("org.enso.runtime"));
    isRuntimeModInBootLayer = hasRuntimeMod;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public Class<?> loadClass(String name) throws ClassNotFoundException {
    return loadClass(name, false);
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
    logger.log(Logger.Level.TRACE, "Loading class {0}", name);
    var l = loadedClasses.get(name);
    if (l != null) {
      logger.log(Logger.Level.TRACE, "Class {0} found in cache", name);
      return l;
    }
    synchronized (this) {
      l = loadedClasses.get(name);
      if (l != null) {
        logger.log(Logger.Level.TRACE, "Class {0} found in cache", name);
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
        logger.log(Logger.Level.TRACE, "Class {0} found, putting in cache", name);
        loadedClasses.put(name, l);
        return l;
      } catch (ClassNotFoundException ex) {
        logger.log(Logger.Level.TRACE, "Class {0} not found, delegating to super", name);
        return super.loadClass(name, resolve);
      } catch (Throwable e) {
        logger.log(Logger.Level.TRACE, "Failure while loading a class: " + e.getMessage(), e);
        throw e;
      }
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
    if (findLibraries != null) {
      try {
        var iop = InteropLibrary.getUncached();
        var res = iop.execute(findLibraries, libname);
        if (iop.isString(res)) {
          return iop.asString(res);
        }
      } catch (InteropException ex) {
        logger.log(Logger.Level.WARNING, "Cannot find " + libname, ex);
      }
    }
    logger.log(Logger.Level.WARNING, "Native library {0} not found in any package", libname);
    return null;
  }

  @Override
  public void close() {
    loadedClasses.clear();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final Object invokeMember(String name, Object[] args)
      throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
    switch (name) {
      case "addPath" -> {
        if (args.length != 1) {
          throw ArityException.create(1, 1, args.length);
        }
        if (args[0] instanceof String path) {
          var file = new File(path);
          try {
            addURL(file.toURI().toURL());
          } catch (MalformedURLException ex) {
            throw UnsupportedTypeException.create(args, "Cannot convert to URL", ex);
          }
        } else {
          throw UnsupportedTypeException.create(args);
        }
      }
      case "findLibraries" -> {
        if (args.length != 1) {
          throw ArityException.create(1, 1, args.length);
        }
        if (InteropLibrary.getUncached().isExecutable(args[0])) {
          this.findLibraries = args[0];
        } else {
          throw UnsupportedTypeException.create(args);
        }
      }
      case "close" -> {
        close();
      }
      default -> throw UnknownIdentifierException.create(name);
    }
    return this;
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  @ExportMessage
  boolean isMemberInvocable(String member) {
    return true;
  }

  @ExportMessage
  boolean isMemberReadable(String member) {
    return true;
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  Object readMember(String member) throws UnknownIdentifierException {
    try {
      var clazz = loadClass(member);
      var ctx = EpbContext.get(null);
      return ctx.getEnv().asHostSymbol(clazz);
    } catch (ClassNotFoundException ex) {
      logger.log(Level.DEBUG, "Cannot find class {0} in host class loader", member);
      throw UnknownIdentifierException.create(member);
    }
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
    return this;
  }

  @ExportMessage
  long getArraySize() {
    return 0;
  }

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  Object readArrayElement(long index) throws InvalidArrayIndexException {
    throw InvalidArrayIndexException.create(index);
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return false;
  }
}
