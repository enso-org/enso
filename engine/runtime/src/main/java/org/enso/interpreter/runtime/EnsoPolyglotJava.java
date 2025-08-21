package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.List;
import org.enso.common.HostEnsoUtils;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.pkg.NativeLibraryFinder;
import org.enso.pkg.Package;

/**
 * Handles a polyglot Java system for loading classes from a single source. <em>Single source</em>
 * is a collection of Java modules/libraries/JARs that belong together.
 */
final class EnsoPolyglotJava {
  private static final System.Logger logger = System.getLogger(EnsoPolyglotJava.class.getName());
  private static final EnsoContext.Extra<CtxData> KEY =
      new EnsoContext.Extra<>(CtxData.class, CtxData::new);

  private final boolean isHostClassLoading;
  private final List<File> pendingPath = new ArrayList<>();
  private Object polyglotJava = this;

  private EnsoPolyglotJava(boolean isHostClassLoading) {
    this.isHostClassLoading = isHostClassLoading;
  }

  static EnsoPolyglotJava find(EnsoContext ctx, org.enso.pkg.Package<?> pkgOrNull) {
    var data = KEY.get(ctx);
    var useGuest = true;
    if (ctx.isHostClassLoading()) {
      if (isHostClassLoadingFor(pkgOrNull)) {
        useGuest = false;
      } else {
        if (pkgOrNull != null) {
          pkgOrNull.warnAotReady(
              () -> {
                logger.log(
                    Level.WARNING,
                    "Package {0} forced to guest classloading. Use --jvm when encountering"
                        + " problems.",
                    logNameForPkg(pkgOrNull));
              });
        }
      }
    }

    if (useGuest) {
      logger.log(Level.DEBUG, "Using guest JVM for {0}", logNameForPkg(pkgOrNull));
      return data.guest;
    } else {
      logger.log(Level.DEBUG, "Using host JVM for {0}", logNameForPkg(pkgOrNull));
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
  private synchronized Object findPolyglotJava(EnsoContext ctx) throws InteropException {
    if (polyglotJava != this) {
      return polyglotJava;
    }
    polyglotJava = createPolyglotJava(ctx);
    while (!pendingPath.isEmpty()) {
      addToClassPath(pendingPath.remove(0));
    }
    try {
      InteropLibrary.getUncached()
          .invokeMember(polyglotJava, "findLibraries", new LibraryResolver());
    } catch (InteropException ex) {
      logger.log(Level.WARNING, "Cannot register findLibraries", ex);
    }
    return polyglotJava;
  }

  /**
   * This method ensure that hosted as well as guest classpath is the same. This is necessary until
   * real isolation between libraries is implemented.
   */
  static void addToClassPath(EnsoContext ctx, Object whoIsIgnored, File path)
      throws InteropException {
    var data = KEY.get(ctx);
    data.hosted.addToClassPath(path);
    data.guest.addToClassPath(path);
  }

  /**
   * Modifies the classpath to use to lookup {@code polyglot java} imports.
   *
   * @param file the file to register
   */
  @CompilerDirectives.TruffleBoundary
  private final synchronized void addToClassPath(File file) throws InteropException {
    if (polyglotJava == this) {
      pendingPath.add(file);
    } else {
      InteropLibrary.getUncached().invokeMember(polyglotJava, "addPath", file.toString());
    }
  }

  private final synchronized void close() {
    if (polyglotJava instanceof TruffleObject closeJava) {
      polyglotJava = null;
      try {
        InteropLibrary.getUncached().invokeMember(closeJava, "close");
      } catch (InteropException ex) {
        logger.log(Level.WARNING, "Cannot close " + closeJava, ex);
      }
    } else {
      polyglotJava = null;
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
        logger.log(Level.ERROR, "Using experimental OtherJvm support!");
        var src = Source.newBuilder("epb", "java:0#guest", "<Bindings>").build();
        var target = ctx.parseInternal(src);
        return target.call();
      }
      if ("espresso".equals(envJava)) {
        var src = Source.newBuilder("java", "<Bindings>", "getbindings.java").build();
        try {
          var java = ctx.parseInternal(src).call();
          logger.log(Level.ERROR, "Using experimental Espresso support!");
          return java;
        } catch (Exception ex) {
          if (ex.getMessage().contains("No language for id java found.")) {
            logger.log(
                Level.ERROR,
                "Environment variable ENSO_JAVA={0}, but {1}",
                new Object[] {envJava, ex.getMessage()});
            logger.log(Level.ERROR, "Copy missing libraries to components directory");
            logger.log(Level.ERROR, "Continuing in regular Java mode");
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

  final TruffleObject loadClass(EnsoContext ctx, String fqn) throws InteropException {
    var raw = InteropLibrary.getUncached().readMember(findPolyglotJava(ctx), fqn);
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
    private EnsoPolyglotJava hosted = new EnsoPolyglotJava(true);
    private EnsoPolyglotJava guest = new EnsoPolyglotJava(false);

    CtxData(EnsoContext ctx) {}
  }
}
