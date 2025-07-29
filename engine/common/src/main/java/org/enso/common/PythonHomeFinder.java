package org.enso.common;

import java.nio.file.Path;

/**
 * Finds directory with unpacked GraalPy resources. It is assumed that these resources are unpacked
 * during build. See {@code org.enso.pyextract.PythonExtract}.
 */
public final class PythonHomeFinder {
  private PythonHomeFinder() {}

  public static Path findPythonHome() {
    assert !HostEnsoUtils.isAot();
    var modPath = getEnsoRuntimeModulePath();
    var componentDir = modPath.getParent();
    var pyHomePath = componentDir.getParent().resolve("python-home");
    var dirExists = pyHomePath.toFile().exists() && pyHomePath.toFile().isDirectory();
    assert dirExists
        : "Python home directory " + pyHomePath + " does not exist or is not a directory.";
    return pyHomePath;
  }

  private static Path getEnsoRuntimeModulePath() {
    var conf = ModuleLayer.boot().configuration();
    var runtimeMod =
        conf.findModule("org.enso.runtime")
            .orElseThrow(
                () ->
                    new IllegalStateException(
                        "Module org.enso.runtime not found in module boot layer."));
    var loc =
        runtimeMod
            .reference()
            .location()
            .orElseThrow(
                () ->
                    new IllegalStateException("Module org.enso.runtime does not have a location."));
    return Path.of(loc);
  }
}
