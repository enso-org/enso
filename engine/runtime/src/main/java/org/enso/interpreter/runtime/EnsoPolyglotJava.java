package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleSafepoint;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Semaphore;
import org.enso.common.HostEnsoUtils;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.pkg.NativeLibraryFinder;
import org.enso.pkg.Package;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handles a polyglot Java system for loading classes from a single source. <em>Single source</em>
 * is a collection of Java modules/libraries/JARs that belong together.
 */
final class EnsoPolyglotJava {
  private static final Logger logger = LoggerFactory.getLogger(EnsoPolyglotJava.class);
  private static final EnsoContext.Extra<CtxData> KEY =
      new EnsoContext.Extra<>(CtxData.class, CtxData::new);

  private final EnsoContext ctx;
  private final boolean isHostClassLoading;
  private final List<File> classPath;

  /**
   * the amount of elements from {@link #classPath} already added to {@link
   * #polyglotJava}. @GuardedBy("lock")
   */
  private int classPathSize;

  private Object polyglotJava = this;
  private Semaphore lock = new Semaphore(1, true);

  /**
   * @param ctx associated conext
   * @param isHostClassLoading do host classloading
   * @param growingPath classpath that may receive new entries incrementally - just make sure the
   *     list is find with multi threaded access
   */
  private EnsoPolyglotJava(EnsoContext ctx, boolean isHostClassLoading, List<File> growingPath) {
    this.ctx = ctx;
    this.isHostClassLoading = isHostClassLoading;
    this.classPath = growingPath;
  }

  /**
   * Performs a lookup for a Java class.
   *
   * @param className the {@code .} separated class name to look for
   * @param collectExceptions reported exceptions are put into this array
   * @return {@code null} on failure or an object representing the {@code className}
   */
  final TruffleObject lookupJavaClass(
      Package<?> requestedBy, String className, Collection<? super Exception> collectExceptions) {
    var binaryName = new StringBuilder(className);
    for (; ; ) {
      var fqn = binaryName.toString();
      try {
        var hostSymbol = loadClass(fqn, requestedBy);

        if (hostSymbol != null) {
          return hostSymbol;
        }

      } catch (RuntimeException | InteropException ex) {
        collectExceptions.add(ex);
      }
      var at = fqn.lastIndexOf('.');
      if (at < 0) {
        break;
      }
      binaryName.setCharAt(at, '$');
    }
    return null;
  }

  @CompilerDirectives.TruffleBoundary
  boolean isOtherObject(Object obj) {
    try {
      var iop = InteropLibrary.getUncached();
      return obj != null
          && iop.hasLanguage(obj)
          && iop.getLanguage(obj).getSimpleName().equals("EpbLanguage");
    } catch (UnsupportedMessageException ex) {
      return false;
    }
  }

  @CompilerDirectives.TruffleBoundary
  boolean isOtherFunction(Object obj) {
    return isOtherObject(obj) && InteropLibrary.getUncached().isExecutable(obj);
  }

  /**
   * Finds proper "polyglot Java" for given package - e.g. Enso library/project.
   *
   * @param ctx the context to query for
   * @param pkgOrNull the library or {@code null} for code outside of any library
   * @return instance of "polyglot Java" for further queries
   */
  static EnsoPolyglotJava find(EnsoContext ctx, org.enso.pkg.Package<?> pkgOrNull) {
    var useGuest = true;
    var data = KEY.get(ctx);
    if (data.isHostClassLoading(pkgOrNull)) {
      if (isHostClassLoadingFor(pkgOrNull)) {
        useGuest = false;
      } else {
        if (pkgOrNull != null) {
          pkgOrNull.checkAotReady(
              () -> {
                logger.warn(
                    "Package {} forced to guest classloading. Use --jvm when encountering"
                        + " problems.",
                    logNameForPkg(pkgOrNull));
              });
        }
      }
    }
    return find(ctx, useGuest);
  }

  static EnsoPolyglotJava find(EnsoContext ctx, boolean guestJava) {
    var data = KEY.get(ctx);
    if (guestJava) {
      return data.guest;
    } else {
      return data.hosted;
    }
  }

  private static String logNameForPkg(Package<?> pkgOrNull) {
    return pkgOrNull == null ? "<none>" : pkgOrNull.libraryName().qualifiedName();
  }

  private static boolean isHostClassLoadingFor(Package<?> pkgOrNull) {
    if (HostEnsoUtils.isAot()) {
      if (pkgOrNull != null && pkgOrNull.isAotReady()) {
        // if the package has been "compiled into" AOT binary
        return true;
      }
    } else {
      // any package can be loaded via host interop in non-AOT mode
      return true;
    }
    return false;
  }

  static void close(EnsoContext ctx) {
    var data = KEY.get(ctx);
    data.hosted.close();
    data.guest.close();
  }

  @CompilerDirectives.TruffleBoundary
  private Object findPolyglotJava() throws InteropException {
    TruffleSafepoint.setBlockedThreadInterruptible(null, Semaphore::acquire, lock);
    try {
      if (polyglotJava instanceof Throwable t) {
        throw ctx.raiseAssertionPanic(null, t.getMessage(), t);
      }
      if (polyglotJava != this) {
        return polyglotJava;
      }
      if (polyglotJava == this) {
        polyglotJava = createPolyglotJava(ctx);
        try {
          InteropLibrary.getUncached()
              .invokeMember(polyglotJava, "findLibraries", new LibraryResolver());
        } catch (InteropException ex) {
          logger.warn("Cannot register findLibraries", ex);
        }
      }
      adjustClassPath();
      return polyglotJava;
    } finally {
      lock.release();
    }
  }

  private void adjustClassPath()
      throws UnknownIdentifierException,
          ArityException,
          UnsupportedMessageException,
          UnsupportedTypeException {
    var size = classPath.size();
    var iop = InteropLibrary.getUncached();
    while (classPathSize < size) {
      // we are the thread to add this classpath element
      var elem = classPath.get(classPathSize);
      iop.invokeMember(polyglotJava, "addPath", elem.toString());
      classPathSize++;
    }
  }

  /**
   * This method ensure that hosted as well as guest classpath is the same. This is necessary until
   * real isolation between libraries is implemented.
   */
  static void addToClassPath(
      EnsoContext ctx, Object whoIsIgnored, File path, boolean polyglotContextEntered)
      throws InteropException {
    var data = KEY.get(ctx);
    data.classPath.add(path);
  }

  @CompilerDirectives.TruffleBoundary
  private final void close() {
    TruffleSafepoint.setBlockedThreadInterruptible(null, Semaphore::acquire, lock);
    try {
      if (polyglotJava instanceof TruffleObject closeJava) {
        polyglotJava = null;
        try {
          InteropLibrary.getUncached().invokeMember(closeJava, "close");
        } catch (InteropException ex) {
          logger.warn("Cannot close " + closeJava, ex);
        }
      } else {
        polyglotJava = null;
      }
    } finally {
      lock.release();
    }
  }

  private Object createPolyglotJava(EnsoContext ctx) throws IllegalStateException {
    if (isHostClassLoading) {
      var src = Source.newBuilder("epb", "java:0#hosted", "<Bindings>").build();
      var target = ctx.parseInternal(src);
      return target.call();
    } else {
      var envJava = System.getenv("ENSO_JAVA");
      if (envJava == null) {
        logger.info("Initializing OtherJvm support!");
        var src = Source.newBuilder("epb", "java:0#guest", "<Bindings>").build();
        var target = ctx.parseInternal(src);
        return target.call();
      }
      if ("espresso".equals(envJava)) {
        var src = Source.newBuilder("java", "<Bindings>", "getbindings.java").build();
        try {
          var java = ctx.parseInternal(src).call();
          logger.error("Using experimental Espresso support!");
          return java;
        } catch (Exception ex) {
          if (ex.getMessage().contains("No language for id java found.")) {
            logger.error(
                "Environment variable ENSO_JAVA={}, but {}",
                new Object[] {envJava, ex.getMessage()});
            logger.error("Copy missing libraries to components directory");
            logger.error("Continuing in regular Java mode");
          } else {
            var ise = new IllegalStateException(ex.getMessage());
            ise.setStackTrace(ex.getStackTrace());
            throw ise;
          }
        }
      } else {
        throw new IllegalStateException(
            "Specify ENSO_JAVA=espresso to use Espresso. Was: " + envJava);
      }
    }
    return null;
  }

  private final TruffleObject loadClass(String fqn, Package<?> requestedBy)
      throws InteropException {
    var raw = InteropLibrary.getUncached().readMember(findPolyglotJava(), fqn);
    logger.debug(
        "Classloading of {} as {} requested by {}",
        fqn,
        raw,
        requestedBy == null ? "null" : requestedBy.libraryName());
    return (TruffleObject) raw;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class LibraryResolver implements TruffleObject {

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object execute(Object[] args) throws ArityException, UnsupportedTypeException {
      if (args.length != 1) {
        throw ArityException.create(1, 1, args.length);
      }
      if (args[0] instanceof String libname) {
        var pkgRepo = EnsoContext.get(null).getPackageRepository();
        for (var pkg : pkgRepo.getLoadedPackagesJava()) {
          var libPath =
              NativeLibraryFinder.findNativeLibrary(libname, pkg, TruffleFileSystem.INSTANCE);
          if (libPath != null) {
            return libPath;
          }
        }
      }
      throw UnsupportedTypeException.create(args);
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }

  private static final class CtxData {
    private final List<File> classPath;
    private final EnsoPolyglotJava hosted;
    private final EnsoPolyglotJava guest;
    private final Map<String, String> hostClassLoading;

    CtxData(EnsoContext ctx) {
      this.classPath = new CopyOnWriteArrayList<>();
      this.hosted = new EnsoPolyglotJava(ctx, true, classPath);
      this.guest = new EnsoPolyglotJava(ctx, false, classPath);
      this.hostClassLoading = new LinkedHashMap<>();
      for (var entry : ctx.getHostClassLoading().split(",")) {
        var libState = entry.split(":");
        switch (libState.length) {
          case 2 -> {
            assert RuntimeOptions.HOST_CLASS_LOADING_HOSTED.equals(libState[1])
                || RuntimeOptions.HOST_CLASS_LOADING_GUEST.equals(libState[1]);
            hostClassLoading.putIfAbsent(libState[0], libState[1]);
          }
          case 1 -> hostClassLoading.putIfAbsent("", libState[0]);
          default ->
              throw new IllegalStateException(
                  "Expecting [<namespace.name>]:hosted|guest, but was: " + entry);
        }
      }
      assert hostClassLoading.containsKey("");
    }

    private boolean isHostClassLoading(Package<?> pkgOrNull) {
      return RuntimeOptions.HOST_CLASS_LOADING_HOSTED.equals(find(pkgOrNull));
    }

    private String find(Package<?> pkgOrNull) {
      if (pkgOrNull != null) {
        var fqn = pkgOrNull.libraryName().toString();
        var set = hostClassLoading.get(fqn);
        if (set != null) {
          return set;
        }
      }
      return hostClassLoading.get("");
    }
  }
}
