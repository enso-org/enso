package org.enso.interpreter.runtime.nativeimage;

import java.lang.invoke.MethodHandle;
import java.nio.file.Files;
import java.nio.file.Path;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.ImageSingletons;

/**
 * Utility class for handling <emph>native library search path</emph> changes at runtime. Changing
 * the search path works only in NI.
 *
 * <p>Semantically the same as changing the {@code java.library.path} system property, and ensuring
 * the property change is reflected by the JVM.
 *
 * <p>Inspired by <a
 * href="https://github.com/Akirathan/native-lib-loader">Akirathan/native-lib-loader</a>
 */
public final class NativeLibrarySearchPath {
  private NativeLibrarySearchPath() {}

  /** Getter for field {@code com.oracle.svm.core.jdk.NativeLibraries#usrPaths}. */
  static MethodHandle usrPathsGetter;

  /** Setter for field {@code com.oracle.svm.core.jdk.NativeLibraries#usrPaths}. */
  static MethodHandle usrPathsSetter;

  /**
   * {@link ImageSingletons singleton} instance of {@code
   * com.oracle.svm.core.jdk.NativeLibrarySupport}. This instance has {@code usrPaths} field that we
   * want to change at runtime. Note that this field needs to be {@code final}, otherwise, NI build
   * fails.
   */
  static final Object nativeLibsInstance = null;

  /**
   * Adds the given {@code path} directory to the search path for native libraries. Semantically, it
   * is equivalent to changing the {@code java.library.path} system property. Only works in Native
   * Image.
   *
   * @param path Directory to add to the search path.
   */
  public static void addToSearchPath(String path) {
    if (ImageInfo.inImageRuntimeCode()) {
      assert Files.isDirectory(Path.of(path));
      addPath(path);
    }
  }

  private static String[] getFieldValue() {
    try {
      return (String[]) usrPathsGetter.invoke(nativeLibsInstance);
    } catch (Throwable e) {
      throw new IllegalStateException(e);
    }
  }

  private static void addPath(String path) {
    var oldValue = getFieldValue();
    var newValue = new String[oldValue.length + 1];
    System.arraycopy(oldValue, 0, newValue, 0, oldValue.length);
    newValue[newValue.length - 1] = path;
    setFieldValue(newValue);
  }

  private static void setFieldValue(String[] newValue) {
    try {
      usrPathsSetter.invoke(nativeLibsInstance, newValue);
    } catch (Throwable e) {
      throw new IllegalStateException(e);
    }
  }
}
