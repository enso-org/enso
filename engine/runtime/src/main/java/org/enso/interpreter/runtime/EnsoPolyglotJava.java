package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
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
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.List;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.pkg.NativeLibraryFinder;

/**
 * Handles a polyglot Java system for loading classes from a single source. <em>Single source</em>
 * is a collection of Java modules/libraries/JARs that belong together.
 */
final class EnsoPolyglotJava {

  private static final System.Logger logger = System.getLogger(EnsoPolyglotJava.class.getName());
  private final TruffleLanguage.Env environment;
  private final boolean isHostClassLoading;
  private final boolean isGuestClassLoading;

  private final List<File> pendingPath = new ArrayList<>();
  private Object polyglotJava = this;

  EnsoPolyglotJava(
      TruffleLanguage.Env environment, boolean isHostClassLoading, boolean isGuestClassLoading) {
    this.environment = environment;
    this.isHostClassLoading = isHostClassLoading;
    this.isGuestClassLoading = isGuestClassLoading;
  }

  @CompilerDirectives.TruffleBoundary
  private synchronized Object findPolyglotJava() throws InteropException {
    if (polyglotJava != this) {
      return polyglotJava;
    }
    polyglotJava = createPolyglotJava();
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
   * Modifies the classpath to use to lookup {@code polyglot java} imports.
   *
   * @param file the file to register
   */
  @CompilerDirectives.TruffleBoundary
  final synchronized void addToClassPath(File file) throws InteropException {
    if (polyglotJava == this) {
      pendingPath.add(file);
    } else {
      InteropLibrary.getUncached().invokeMember(polyglotJava, "addPath", file.toString());
    }
  }

  final synchronized void close() {
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

  private Object createPolyglotJava() throws IllegalStateException {
    if (isHostClassLoading) {
      var src = Source.newBuilder("epb", "java:0#hosted", "<Bindings>").build();
      var target = environment.parseInternal(src);
      return target.call();
    }
    if (isGuestClassLoading) {
      var envJava = System.getenv("ENSO_JAVA");
      if (envJava == null) {
        logger.log(Level.ERROR, "Using experimental OtherJvm support!");
        var src = Source.newBuilder("epb", "java:0#guest", "<Bindings>").build();
        var target = environment.parseInternal(src);
        return target.call();
      }
      if ("espresso".equals(envJava)) {
        var src = Source.newBuilder("java", "<Bindings>", "getbindings.java").build();
        try {
          var java = environment.parsePublic(src).call();
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

  final TruffleObject loadClass(String fqn)
      throws UnsupportedMessageException, UnknownIdentifierException, InteropException {
    var raw = InteropLibrary.getUncached().readMember(findPolyglotJava(), fqn);
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
}
