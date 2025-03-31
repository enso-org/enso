package org.enso.change.directory;

import static org.junit.Assert.assertEquals;

import org.graalvm.nativeimage.ImageInfo;
import org.junit.Test;

public class TestChangeDirectory {
  public TestChangeDirectory() {}

  private static void ensureInNativeImage() {
    var inNativeImage = ImageInfo.inImageRuntimeCode();
    assert inNativeImage;
  }

  @Test
  public void curDir() {
    ensureInNativeImage();
    var nativeApi = WorkingDirectory.getForCurrentPlatform();
    var curDir = nativeApi.currentWorkingDir();
    var expectedDir = System.getProperty("user.dir");
    assertEquals(expectedDir, curDir);
  }
}
