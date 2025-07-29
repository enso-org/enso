package org.enso.common;

import java.nio.file.Path;
import org.graalvm.nativeimage.ImageInfo;

/**
 * Finds directory with unpacked GraalPy resources. It is assumed that these resources are unpacked
 * during build. See {@code org.enso.pyextract.PythonExtract}.
 */
public final class PythonHomeFinder {
  private PythonHomeFinder() {}

  public static Path findPythonHome() {
    if (ImageInfo.inImageRuntimeCode()) {
      throw new UnsupportedOperationException("unimplemented");
    } else {
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
                      new IllegalStateException(
                          "Module org.enso.runtime does not have a location."));
      var modPath = Path.of(loc);
      var componentDir = modPath.getParent();
      var pyHomePath = componentDir.getParent().resolve("python-home");
      return pyHomePath;
    }
  }
}
