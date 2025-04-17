package org.enso.base_test_helpers;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import org.graalvm.nativeimage.ImageInfo;

public final class ExecutableLocation {
  private ExecutableLocation() {}

  /**
   * Returns absolute path to the enso executable location.
   * Works only in NI. Otherwise returns null.
   */
  public static String getExecutableLocation() {
    if (!ImageInfo.inImageRuntimeCode()) {
      return null;
    }
    var codeSource = ExecutableLocation.class.getProtectionDomain().getCodeSource();
    assert codeSource != null;
    var loc = codeSource.getLocation();
    try {
      var file = new File(loc.toURI());
      return file.getCanonicalPath();
    } catch (URISyntaxException | IOException e) {
      throw new AssertionError(e);
    }
  }
}
