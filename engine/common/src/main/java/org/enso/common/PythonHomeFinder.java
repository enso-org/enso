package org.enso.common;

import java.nio.file.Path;

/**
 * Finds directory with unpacked GraalPy resources. It is assumed that these resources are unpacked
 * during build. See {@code org.enso.pyextract.PythonExtract}.
 */
public final class PythonHomeFinder {
  private PythonHomeFinder() {}

  /**
   * Finds directory with unpacked GraalPy resources. It is assumed that these resources are
   * unpacked during build. See {@code org.enso.pyextract.PythonExtract}. Can only be found if
   * Engine runner was started with `org.enso.runtime` module on the module path.
   *
   * @return Path to existing directory with GraalPy resources or null if it cannot be found.
   */
  public static Path findPythonHome() {
    if (HostEnsoUtils.isAot()) {
      return null;
    }
    var modPath = getEnsoRuntimeModulePath();
    if (modPath == null) {
      return null;
    }
    var componentDir = modPath.getParent();
    var pyHomePath = componentDir.getParent().resolve("python-home");
    var dirExists = pyHomePath.toFile().exists() && pyHomePath.toFile().isDirectory();
    return dirExists ? pyHomePath : null;
  }

  private static Path getEnsoRuntimeModulePath() {
    var conf = ModuleLayer.boot().configuration();
    var runtimeMod = conf.findModule("org.enso.runtime");
    if (runtimeMod.isEmpty()) {
      return null;
    }
    var loc =
        runtimeMod
            .get()
            .reference()
            .location()
            .orElseThrow(
                () ->
                    new IllegalStateException("Module org.enso.runtime does not have a location."));
    return Path.of(loc);
  }
}
