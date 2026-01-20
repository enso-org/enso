package org.enso.os.environment.chdir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeFalse;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.enso.common.Platform;
import org.graalvm.nativeimage.ImageInfo;
import org.junit.AfterClass;
import org.junit.Before;
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

  @Before
  public void ensureCwdIsAtTmpDir() throws IOException {
    var rootPath = TMP_DIR.getRoot().getCanonicalPath();
    nativeApi.changeWorkingDir(rootPath);
    assertEquals("CWD is correct", rootPath, nativeApi.currentWorkingDir());
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

  @Test
  public void testFindProjectRoot_AbsolutePath() throws IOException {
    var projDir = TMP_DIR.newFolder().toPath();
    createDummyProject(projDir);
    var main = projDir.resolve("src").resolve("Main.enso");
    var mainAbsPath = main.toFile().getCanonicalPath();
    var expectedProjRoot = projDir.toFile().getCanonicalPath();
    var actualProjRoot = nativeApi.findProjectRoot(mainAbsPath);
    assertEquals(
        "Should be able to find project root for " + mainAbsPath, expectedProjRoot, actualProjRoot);
  }

  /** Create Proj directory in the current working directory and try to determine its root. */
  @Test
  public void testFindProjectRoot_RelativePath() throws IOException {
    findProjectRootRelativePath("");
  }

  @Test
  public void testFindProjectRoot_RelativePathTrailingSlash() throws IOException {
    findProjectRootRelativePath("/");
  }

  @Test
  public void testFindProjectRoot_RelativePathTrailingSeparatorChar() throws IOException {
    findProjectRootRelativePath(java.io.File.separator);
  }

  private void findProjectRootRelativePath(String suffix) throws IOException {
    var projDir = TMP_DIR.newFolder("Proj").toPath();
    createDummyProject(projDir);
    var main = projDir.resolve("src").resolve("Main.enso");
    var relative = TMP_DIR.getRoot().toPath().relativize(main) + suffix;
    var expectedProjRoot = projDir.toFile().getCanonicalPath();
    var actualProjRoot = nativeApi.findProjectRoot(relative);
    assertEquals(
        "Should be able to find project root for " + relative, expectedProjRoot, actualProjRoot);
    deleteRecursively(projDir);
  }

  @Test
  public void findProjectWithSlashes() throws IOException {
    findProjectWith("SimpleSlash", "SimpleSlash/SubPrj");
  }

  @Test
  public void findProjectWithEvenTrailingSlashes() throws IOException {
    findProjectWith("Trailing", "Trailing/SubPrj");
  }

  @Test
  public void findProjectWithSlashesToMain() throws IOException {
    findProjectWith("MainFile", "MainFile/SubPrj/src/Main.enso");
  }

  private void findProjectWith(String container, String resolvePath) throws IOException {
    var subPrjDir = TMP_DIR.newFolder(container, "SubPrj").toPath();
    createDummyProject(subPrjDir);
    var expectedProjRoot = subPrjDir.toFile().getCanonicalPath();
    var actualProjRoot = nativeApi.findProjectRoot(resolvePath);
    assertEquals(
        "Should be able to find project root for " + "Contain/SubPrj",
        expectedProjRoot,
        actualProjRoot);

    deleteRecursively(subPrjDir.getParent().toAbsolutePath());
  }

  private static void deleteRecursively(Path projDir) throws IOException {
    Files.walkFileTree(
        projDir,
        new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
              throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
          }
        });
    var dir = projDir.toFile();
    if (dir.exists()) {
      assertTrue("Directory can be deleted at the end: " + projDir, dir.delete());
    }
  }

  @Test
  public void testNoProjectRoot() throws IOException {
    var tmpDirAbsPath = TMP_DIR.newFolder().getCanonicalPath();
    var actualProjRoot = nativeApi.findProjectRoot(tmpDirAbsPath);
    assertNull("Should not be able to find project root in " + tmpDirAbsPath, actualProjRoot);
  }

  private static void createDummyProject(Path projDir) throws IOException {
    var projFile = projDir.toFile();
    projFile.mkdirs();
    assertTrue("Directory created " + projDir, projFile.isDirectory());
    Files.write(
        projDir.resolve("package.yaml"),
        List.of("name: " + projDir.getFileName(), "version: 0.0.0-dev"),
        StandardOpenOption.CREATE_NEW);
    var dirCreated = projDir.resolve("src").toFile().mkdirs();
    assertTrue(dirCreated);
    var main = projDir.resolve("src").resolve("Main.enso");
    Files.write(main, List.of("main = 42"), StandardOpenOption.CREATE_NEW);
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
