package org.enso.os.environment.chdir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.util.concurrent.TimeUnit;
import org.enso.common.Platform;
import org.graalvm.nativeimage.ImageInfo;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class TestChangeDirectory {
  @ClassRule public static final TemporaryFolder TMP_DIR = new TemporaryFolder();

  private static WorkingDirectory nativeApi;

  @BeforeClass
  public static void init() {
    ensureInNativeImage();
    nativeApi = WorkingDirectory.getInstance();
  }

  @AfterClass
  public static void dispose() {
    nativeApi = null;
  }

  @Test
  public void curDir() {
    var curDir = nativeApi.currentWorkingDir();
    var expectedDir = System.getProperty("user.dir");
    assertEquals(expectedDir, curDir);
  }

  @Test
  public void curDir_IsSameAsPwdOnUnix() throws IOException, InterruptedException {
    ensureOnUnix();
    var curDir = nativeApi.currentWorkingDir();
    var pwd = invokePwd();
    assertEquals(pwd, curDir);
  }

  @Test
  public void changeDir() throws IOException {
    var tmpDir = TMP_DIR.newFolder().toPath();
    var tmpDirAbs = tmpDir.toAbsolutePath().toRealPath().toString();
    var succeeded = nativeApi.changeWorkingDir(tmpDirAbs);
    assertTrue(succeeded);
    var curDir = nativeApi.currentWorkingDir();
    assertEquals(tmpDirAbs, curDir);
  }

  @Test
  public void changeDir_NonExistingDir() throws IOException {
    var tmpDir = TMP_DIR.newFolder().toPath();
    var wasDeleted = tmpDir.toFile().delete();
    assertTrue(wasDeleted);
    var succeeded = nativeApi.changeWorkingDir(tmpDir.toAbsolutePath().toString());
    assertFalse(succeeded);
  }

  @Test
  public void changeDir_Symlink() throws IOException {
    var tmpDir = TMP_DIR.newFolder().toPath();
    var realDir = tmpDir.resolve("real-dir");
    var dirCreated = realDir.toFile().mkdir();
    assertTrue(dirCreated);
    var symlink = tmpDir.resolve("symlink");
    try {
      Files.createSymbolicLink(symlink, realDir);
    } catch (UnsupportedOperationException e) {
      assumeFalse("Symlink creation is not supported on this platform, skipping the test", true);
    }
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
    var tmpDir = TMP_DIR.newFolder().toPath();
    var dir = tmpDir.resolve("..").toRealPath().toString();
    var file = tmpDir.getFileName().toString();
    var dirExists = nativeApi.exists(dir, file);
    assertTrue("Should exist: dir=" + dir + ", file=" + file + ", whole path=" + tmpDir, dirExists);
  }

  @Test
  public void testDoesNotExist() throws IOException {
    var tmpDir = TMP_DIR.newFolder().toPath();
    var wasDeleted = tmpDir.toFile().delete();
    assertTrue(wasDeleted);
    var dir = tmpDir.subpath(0, tmpDir.getNameCount() - 1);
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
}
