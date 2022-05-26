// Copied unmodified from
// https://github.com/openpnp/opencv/blob/v4.5.1-0/src/main/java/nu/pattern/OpenCV.java
// to allow library loading on GraalVM.
package org.enso.image.opencv;

import org.opencv.core.Core;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public class OpenCV {

  private static final Logger logger = Logger.getLogger(OpenCV.class.getName());

  static enum OS {
    OSX("^[Mm]ac OS X$"),
    LINUX("^[Ll]inux$"),
    WINDOWS("^[Ww]indows.*");

    private final Set<Pattern> patterns;

    private OS(final String... patterns) {
      this.patterns = new HashSet<Pattern>();

      for (final String pattern : patterns) {
        this.patterns.add(Pattern.compile(pattern));
      }
    }

    private boolean is(final String id) {
      for (final Pattern pattern : patterns) {
        if (pattern.matcher(id).matches()) {
          return true;
        }
      }
      return false;
    }

    public static OS getCurrent() {
      final String osName = System.getProperty("os.name");

      for (final OS os : OS.values()) {
        if (os.is(osName)) {
          logger.log(
              Level.FINEST, "Current environment matches operating system descriptor \"{0}\".", os);
          return os;
        }
      }

      throw new UnsupportedOperationException(
          String.format("Operating system \"%s\" is not supported.", osName));
    }
  }

  static enum Arch {
    X86_32("i386", "i686", "x86"),
    X86_64("amd64", "x86_64"),
    ARMv8("arm");

    private final Set<String> patterns;

    private Arch(final String... patterns) {
      this.patterns = new HashSet<String>(Arrays.asList(patterns));
    }

    private boolean is(final String id) {
      return patterns.contains(id);
    }

    public static Arch getCurrent() {
      final String osArch = System.getProperty("os.arch");

      for (final Arch arch : Arch.values()) {
        if (arch.is(osArch)) {
          logger.log(
              Level.FINEST, "Current environment matches architecture descriptor \"{0}\".", arch);
          return arch;
        }
      }

      throw new UnsupportedOperationException(
          String.format("Architecture \"%s\" is not supported.", osArch));
    }
  }

  private static class UnsupportedPlatformException extends RuntimeException {
    private UnsupportedPlatformException(final OS os, final Arch arch) {
      super(
          String.format(
              "Operating system \"%s\" and architecture \"%s\" are not supported.", os, arch));
    }
  }

  private static class TemporaryDirectory {
    static final String OPENCV_PREFIX = "opencv_openpnp";
    final Path path;

    public TemporaryDirectory() {
      try {
        path = Files.createTempDirectory(OPENCV_PREFIX);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    public Path getPath() {
      return path;
    }

    public TemporaryDirectory deleteOldInstancesOnStart() {
      Path tempDirectory = path.getParent();

      for (File file : tempDirectory.toFile().listFiles()) {
        if (file.isDirectory() && file.getName().startsWith(OPENCV_PREFIX)) {
          try {
            delete(file.toPath());
          } catch (RuntimeException e) {
            if (e.getCause() instanceof AccessDeniedException) {
              logger.fine(
                  "Failed delete a previous instance of the OpenCV binaries, "
                      + "likely in use by another program: ");
            }
          }
        }
      }

      return this;
    }

    public TemporaryDirectory markDeleteOnExit() {
      Runtime.getRuntime()
          .addShutdownHook(
              new Thread() {
                @Override
                public void run() {
                  delete();
                }
              });

      return this;
    }

    private void delete(Path path) {
      if (!Files.exists(path)) {
        return;
      }

      try {
        Files.walkFileTree(
            path,
            new SimpleFileVisitor<Path>() {
              @Override
              public FileVisitResult postVisitDirectory(final Path dir, final IOException e)
                  throws IOException {
                Files.deleteIfExists(dir);
                return super.postVisitDirectory(dir, e);
              }

              @Override
              public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs)
                  throws IOException {
                Files.deleteIfExists(file);
                return super.visitFile(file, attrs);
              }
            });
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    public void delete() {
      delete(path);
    }
  }

  /**
   * Exactly once per {@link ClassLoader}, attempt to load the native library (via {@link
   * System#loadLibrary(String)} with {@link Core#NATIVE_LIBRARY_NAME}). If the first attempt fails,
   * the native binary will be extracted from the classpath to a temporary location (which gets
   * cleaned up on shutdown), that location is added to the {@code java.library.path} system
   * property and {@link ClassLoader#usr_paths}, and then another call to load the library is made.
   * Note this method uses reflection to gain access to private memory in {@link ClassLoader} as
   * there's no documented method to augment the library path at runtime. Spurious calls are safe.
   */
  public static void loadShared() {
    SharedLoader.getInstance();
  }

  /**
   * @see <a
   *     href="http://en.wikipedia.org/wiki/Initialization-on-demand_holder_idiom">Initialization-on-demand
   *     holder idiom</a>
   */
  private static class SharedLoader {
    // Class loader error messages indicating OpenCV is not found on java.library.path
    private static final List<String> errorMessages =
        Arrays.asList(
            String.format("no %s in java.library.path", Core.NATIVE_LIBRARY_NAME),
            String.format("%s (Not found in java.library.path)", Core.NATIVE_LIBRARY_NAME));

    private Path libraryPath;

    private SharedLoader() {
      try {
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
        logger.log(
            Level.FINEST,
            "Loaded existing OpenCV library \"{0}\" from library path.",
            Core.NATIVE_LIBRARY_NAME);
      } catch (final UnsatisfiedLinkError ule) {

        /* Only update the library path and load if the original error indicates it's missing from the library path. */
        if (ule == null || !openCVNotFoundInJavaLibraryPath(ule.getMessage())) {
          logger.log(Level.FINEST, String.format("Encountered unexpected loading error."), ule);
          throw ule;
        }

        /**
         * In Java >= 12 it is no longer possible to use addLibraryPath, which modifies the
         * ClassLoader's static usr_paths field. There does not seem to be any way around this so we
         * fall back to loadLocally() and return.
         */
        if (Double.parseDouble(System.getProperty("java.specification.version")) >= 12) {
          logger.log(
              Level.SEVERE,
              "OpenCV.loadShared() is not supported in Java >= 12. Falling back to OpenCV.loadLocally().");
          OpenCV.loadLocally();
          return;
        }

        /* Retain this path for cleaning up the library path later. */
        this.libraryPath = extractNativeBinary();

        addLibraryPath(libraryPath.getParent());
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME);

        logger.log(
            Level.FINEST,
            "OpenCV library \"{0}\" loaded from extracted copy at \"{1}\".",
            new Object[] {
              Core.NATIVE_LIBRARY_NAME, System.mapLibraryName(Core.NATIVE_LIBRARY_NAME)
            });
      }
    }

    /**
     * Check if any error fragment is contained in the errorMessage
     *
     * @param errorMessage the message to check
     * @return true if any error fragment matches, false otherwise
     */
    private boolean openCVNotFoundInJavaLibraryPath(String errorMessage) {
      for (String errorFragment : errorMessages) {
        if (errorMessage.contains(errorFragment)) {
          return true;
        }
      }

      return false;
    }

    /** Cleans up patches done to the environment. */
    @Override
    protected void finalize() throws Throwable {
      super.finalize();

      if (null == libraryPath) {
        return;
      }

      removeLibraryPath(libraryPath.getParent());
    }

    private static class Holder {
      private static final SharedLoader INSTANCE = new SharedLoader();
    }

    public static SharedLoader getInstance() {
      return Holder.INSTANCE;
    }

    /**
     * Adds the provided {@link Path}, normalized, to the {@link ClassLoader#usr_paths} array, as
     * well as to the {@code java.library.path} system property. Uses the reflection API to make the
     * field accessible, and may be unsafe in environments with a security policy.
     *
     * @see <a href="http://stackoverflow.com/q/15409223">Adding new paths for native libraries at
     *     runtime in Java</a>
     */
    private static void addLibraryPath(final Path path) {
      final String normalizedPath = path.normalize().toString();

      try {
        final Field field = ClassLoader.class.getDeclaredField("usr_paths");
        field.setAccessible(true);

        final Set<String> userPaths = new HashSet<>(Arrays.asList((String[]) field.get(null)));
        userPaths.add(normalizedPath);

        field.set(null, userPaths.toArray(new String[userPaths.size()]));

        System.setProperty(
            "java.library.path",
            System.getProperty("java.library.path") + File.pathSeparator + normalizedPath);

        logger.log(
            Level.FINEST,
            "System library path now \"{0}\".",
            System.getProperty("java.library.path"));
      } catch (IllegalAccessException e) {
        throw new RuntimeException("Failed to get permissions to set library path");
      } catch (NoSuchFieldException e) {
        throw new RuntimeException("Failed to get field handle to set library path");
      }
    }

    /**
     * Removes the provided {@link Path}, normalized, from the {@link ClassLoader#usr_paths} array,
     * as well as to the {@code java.library.path} system property. Uses the reflection API to make
     * the field accessible, and may be unsafe in environments with a security policy.
     */
    private static void removeLibraryPath(final Path path) {
      final String normalizedPath = path.normalize().toString();

      try {
        final Field field = ClassLoader.class.getDeclaredField("usr_paths");
        field.setAccessible(true);

        final Set<String> userPaths = new HashSet<>(Arrays.asList((String[]) field.get(null)));
        userPaths.remove(normalizedPath);

        field.set(null, userPaths.toArray(new String[userPaths.size()]));

        System.setProperty(
            "java.library.path",
            System.getProperty("java.library.path")
                .replace(File.pathSeparator + path.normalize().toString(), ""));
      } catch (IllegalAccessException e) {
        throw new RuntimeException("Failed to get permissions to set library path");
      } catch (NoSuchFieldException e) {
        throw new RuntimeException("Failed to get field handle to set library path");
      }
    }
  }

  /**
   * Exactly once per {@link ClassLoader}, extract the native binary from the classpath to a
   * temporary location (which gets cleaned up on shutdown), and load that binary (via {@link
   * System#load(String)}). Spurious calls are safe.
   */
  public static void loadLocally() {
    LocalLoader.getInstance();
  }

  private static class LocalLoader {
    private LocalLoader() {
      /* Retain this path for cleaning up later. */
      final Path libraryPath = extractNativeBinary();
      System.load(libraryPath.normalize().toString());

      logger.log(
          Level.FINEST,
          "OpenCV library \"{0}\" loaded from extracted copy at \"{1}\".",
          new Object[] {Core.NATIVE_LIBRARY_NAME, System.mapLibraryName(Core.NATIVE_LIBRARY_NAME)});
    }

    private static class Holder {
      private static final LocalLoader INSTANCE = new LocalLoader();
    }

    public static LocalLoader getInstance() {
      return Holder.INSTANCE;
    }
  }

  /**
   * Selects the appropriate packaged binary, extracts it to a temporary location (which gets
   * deleted when the JVM shuts down), and returns a {@link Path} to that file.
   */
  private static Path extractNativeBinary() {
    final OS os = OS.getCurrent();
    final Arch arch = Arch.getCurrent();
    return extractNativeBinary(os, arch);
  }

  /**
   * Extracts the packaged binary for the specified platform to a temporary location (which gets
   * deleted when the JVM shuts down), and returns a {@link Path} to that file.
   */
  private static Path extractNativeBinary(final OS os, final Arch arch) {
    final String location;

    switch (os) {
      case LINUX:
        switch (arch) {
          case X86_64:
            location = "/nu/pattern/opencv/linux/x86_64/libopencv_java451.so";
            break;
          case ARMv8:
            location = "/nu/pattern/opencv/linux/ARMv8/libopencv_java451.so";
            break;
          default:
            throw new UnsupportedPlatformException(os, arch);
        }
        break;
      case OSX:
        switch (arch) {
          case X86_64:
            location = "/nu/pattern/opencv/osx/x86_64/libopencv_java451.dylib";
            break;
          default:
            throw new UnsupportedPlatformException(os, arch);
        }
        break;
      case WINDOWS:
        switch (arch) {
          case X86_32:
            location = "/nu/pattern/opencv/windows/x86_32/opencv_java451.dll";
            break;
          case X86_64:
            location = "/nu/pattern/opencv/windows/x86_64/opencv_java451.dll";
            break;
          default:
            throw new UnsupportedPlatformException(os, arch);
        }
        break;
      default:
        throw new UnsupportedPlatformException(os, arch);
    }

    logger.log(Level.FINEST, "Selected native binary \"{0}\".", location);

    final InputStream binary = OpenCV.class.getResourceAsStream(location);
    final Path destination;

    // Do not try to delete the temporary directory on the close if Windows
    // because there will be a write lock on the file which will cause an
    // AccessDeniedException. Instead, try to delete existing instances of
    // the temporary directory before extracting.
    if (OS.WINDOWS.equals(os)) {
      destination =
          new TemporaryDirectory()
              .deleteOldInstancesOnStart()
              .getPath()
              .resolve("./" + location)
              .normalize();
    } else {
      destination =
          new TemporaryDirectory()
              .markDeleteOnExit()
              .getPath()
              .resolve("./" + location)
              .normalize();
    }

    try {
      logger.log(Level.FINEST, "Copying native binary to \"{0}\".", destination);
      Files.createDirectories(destination.getParent());
      Files.copy(binary, destination);
      binary.close();
    } catch (final IOException ioe) {
      throw new IllegalStateException(
          String.format("Error writing native library to \"%s\".", destination), ioe);
    }

    logger.log(Level.FINEST, "Extracted native binary to \"{0}\".", destination);

    return destination;
  }
}
