package org.enso.change.directory;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import org.graalvm.nativeimage.ImageInfo;
import org.junit.Test;

public class TestChangeDirectory {
  public TestChangeDirectory() {}

  private static void ensureInNativeImage() {
    var inNativeImage = ImageInfo.inImageRuntimeCode();
    assumeTrue(
        "This test should only be run in a native image. Please run it in a native image.",
        inNativeImage);
  }

  @Test
  public void curDir() {
    ensureInNativeImage();
    var nativeApi = WorkingDirectory.getForCurrentPlatform();
    var curDir = nativeApi.currentWorkingDir();
    var expectedDir = System.getProperty("user.dir");
    assertEquals(expectedDir, curDir);
  }

  @Test
  public void changeDir() throws IOException {
    ensureInNativeImage();
    var tmpDir = Files.createTempDirectory("TestChangeDirectory");
    var tmpDirAbs = tmpDir.toAbsolutePath().toString();
    var nativeApi = WorkingDirectory.getForCurrentPlatform();
    var succeeded = nativeApi.changeWorkingDir(tmpDirAbs);
    assertTrue(succeeded);
    var curDir = nativeApi.currentWorkingDir();
    assertEquals(tmpDirAbs, curDir);
  }
}
