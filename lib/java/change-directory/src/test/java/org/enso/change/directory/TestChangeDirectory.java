package org.enso.change.directory;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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

  @Test
  public void changeDir_NonExistingDir() throws IOException {
    var tmpDir = Files.createTempDirectory("changeDir_NonExistingDir");
    var wasDeleted = tmpDir.toFile().delete();
    assertTrue(wasDeleted);
    var nativeApi = WorkingDirectory.getForCurrentPlatform();
    var succeeded = nativeApi.changeWorkingDir(tmpDir.toAbsolutePath().toString());
    assertFalse(succeeded);
  }

  @Test
  public void testExists() throws IOException {
    ensureInNativeImage();
    var tmpDir = Files.createTempDirectory("TestChangeDirectory_testExists");
    var nativeApi = WorkingDirectory.getForCurrentPlatform();
    var dir = tmpDir.resolve("..").toRealPath().toString();
    var file = tmpDir.getFileName().toString();
    var dirExists = nativeApi.exists(dir, file);
    assertTrue("Should exist: dir=" + dir + ", file=" + file + ", whole path=" + tmpDir, dirExists);
  }

  @Test
  public void testDoesNotExist() throws IOException {
    var tmpDir = Files.createTempDirectory("TestChangeDirectory_testDoesNotExist");
    var wasDeleted = tmpDir.toFile().delete();
    assertTrue(wasDeleted);
    var dir = tmpDir.subpath(0, tmpDir.getNameCount() - 1);
    var nativeApi = WorkingDirectory.getForCurrentPlatform();
    var dirExists =
        nativeApi.exists(dir.toAbsolutePath().toString(), tmpDir.getFileName().toString());
    assertFalse(dirExists);
  }
}
