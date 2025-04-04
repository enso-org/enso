package org.enso.change.directory;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.util.concurrent.TimeUnit;
import org.enso.common.Platform;
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

  private static void ensureOnUnix() {
    var onUnix =
        switch (Platform.getOperatingSystem()) {
          case MACOS, LINUX -> true;
          case WINDOWS -> false;
        };
    assumeTrue(
        "This test should only be run on Unix-like systems. Please run it on a Unix-like system.",
        onUnix);
  }

  @Test
  public void curDir() {
    ensureInNativeImage();
    var nativeApi = WorkingDirectories.getCurrent();
    var curDir = nativeApi.currentWorkingDir();
    var expectedDir = System.getProperty("user.dir");
    assertEquals(expectedDir, curDir);
  }

  @Test
  public void curDir_IsSameAsPwdOnUnix() throws IOException, InterruptedException {
    ensureInNativeImage();
    ensureOnUnix();
    var nativeApi = WorkingDirectories.getCurrent();
    var curDir = nativeApi.currentWorkingDir();
    var pwd = invokePwd();
    assertEquals(pwd, curDir);
  }

  @Test
  public void changeDir() throws IOException {
    ensureInNativeImage();
    var tmpDir = Files.createTempDirectory("TestChangeDirectory");
    var tmpDirAbs = tmpDir.toAbsolutePath().toRealPath().toString();
    var nativeApi = WorkingDirectories.getCurrent();
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
    var nativeApi = WorkingDirectories.getCurrent();
    var succeeded = nativeApi.changeWorkingDir(tmpDir.toAbsolutePath().toString());
    assertFalse(succeeded);
  }

  @Test
  public void changeDir_Symlink() throws IOException {
    ensureInNativeImage();
    ensureOnUnix();
    var tmpDir = Files.createTempDirectory("changeDir_Symlink");
    var realDir = tmpDir.resolve("real-dir");
    var dirCreated = realDir.toFile().mkdir();
    assertTrue(dirCreated);
    var symlink = tmpDir.resolve("symlink");
    Files.createSymbolicLink(symlink, realDir);
    var nativeApi = WorkingDirectories.getCurrent();
    var realDirPath = realDir.toAbsolutePath().toRealPath().toString();
    var symLinkPath = symlink.toAbsolutePath().toRealPath().toString();
    var dirChanged = nativeApi.changeWorkingDir(symLinkPath);
    assertTrue(dirChanged);
    var curDir = nativeApi.currentWorkingDir();
    assertEquals(
        "currentWorkingDir should report real path, with resolved symlinks", realDirPath, curDir);
  }

  @Test
  public void testExists() throws IOException {
    ensureInNativeImage();
    var tmpDir = Files.createTempDirectory("TestChangeDirectory_testExists");
    var nativeApi = WorkingDirectories.getCurrent();
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
    var nativeApi = WorkingDirectories.getCurrent();
    var dirExists =
        nativeApi.exists(dir.toAbsolutePath().toString(), tmpDir.getFileName().toString());
    assertFalse(dirExists);
  }

  private String invokePwd() throws IOException, InterruptedException {
    var process = new ProcessBuilder("pwd").start();
    process.waitFor(3, TimeUnit.SECONDS);
    var pwd =
        new String(
            process.getInputStream().readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
    return pwd.trim();
  }
}
