package org.enso.pkg;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import org.enso.filesystem.FileSystem;

/**
 * Helper class to find native libraries in packages. The search algorithm complies to the <a
 * href="https://bits.netbeans.org/23/javadoc/org-openide-modules/org/openide/modules/doc-files/api.html#jni">NetBeans
 * JNI specification</a>.
 */
public final class NativeLibraryFinder {

  private NativeLibraryFinder() {}

  /**
   * Tries to find native library in the given package.
   *
   * @param libName the name of the library to find, without platform specific prefix or suffix.
   * @param pkg the package to search in.
   * @return null if not found, absolute path otherwise.
   */
  public static <T> String findNativeLibrary(String libName, Package<T> pkg, FileSystem<T> fs) {
    var libNameWithSuffix = System.mapLibraryName(libName);
    for (var dir : searchPath(pkg, fs)) {
      if (!fs.exists(dir)) {
        return null;
      }
      var nativeLib = fs.getChild(dir, libNameWithSuffix);
      if (fs.exists(nativeLib)) {
        return fs.getAbsolutePath(nativeLib);
      }
    }
    return null;
  }

  /** Returns set of native libraries for the given package for the current OS and architecture. */
  public static <T> Set<T> listAllNativeLibraries(Package<T> pkg, FileSystem<T> fs) {
    var nativeLibs = new HashSet<T>();
    for (var dir : searchPath(pkg, fs)) {
      if (!fs.exists(dir)) {
        return nativeLibs;
      }
      try {
        fs.list(dir)
            .forEach(
                file -> {
                  var fname = fs.getName(file);
                  if (fs.isRegularFile(file) && fname.endsWith(nativeLibSuffix())) {
                    nativeLibs.add(file);
                  }
                });
      } catch (IOException e) {
        throw new IllegalStateException(e);
      }
    }
    return nativeLibs;
  }

  private static String nativeLibSuffix() {
    var libName = System.mapLibraryName("");
    return libName.substring(libName.lastIndexOf('.'));
  }

  private static <T> List<T> searchPath(Package<T> pkg, FileSystem<T> fs) {
    var searchPath = new ArrayList<T>();
    var arch = arch();
    var osName = simpleOsName();
    var libDir = pkg.nativeLibraryDir();
    searchPath.add(libDir);
    searchPath.add(fs.getChild(libDir, arch));
    searchPath.add(fs.getChild(fs.getChild(libDir, arch), osName));
    return searchPath;
  }

  private static String simpleOsName() {
    var osName = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);
    if (osName.contains(" ")) {
      // Strip version
      osName = osName.substring(0, osName.indexOf(' '));
    }
    if (osName.contains("linux")) {
      return "linux";
    } else if (osName.contains("mac")) {
      return "macos";
    } else if (osName.contains("windows")) {
      return "windows";
    } else {
      throw new IllegalStateException("Unsupported OS: " + osName);
    }
  }

  private static String arch() {
    var arch = System.getProperty("os.arch").toLowerCase(Locale.ENGLISH);
    return arch.replace("x86_64", "amd64");
  }
}
