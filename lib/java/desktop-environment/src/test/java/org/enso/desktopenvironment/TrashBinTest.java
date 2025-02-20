package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class TrashBinTest {

  private static final TrashBin TRASH_BIN = Platform.getOperatingSystem().getTrashBin();

  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();

  @Test
  public void isSupported() {
    if (isEnabled()) {
      Assert.assertTrue(TRASH_BIN.isSupported());
    }
  }

  @Test
  public void moveToTrashFile() throws IOException {
    if (isEnabled()) {
      var path = createTempFile(temporaryFolder);

      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
      Assert.assertFalse(TRASH_BIN.moveToTrash(path));
    }
  }

  @Test
  public void moveToTrashSameFile() throws IOException {
    if (isEnabled()) {
      var path = createTempFile(temporaryFolder);

      Assert.assertTrue(TRASH_BIN.moveToTrash(path));

      Files.writeString(path, "");
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
    }
  }

  @Test
  public void moveToTrashDirectory() throws IOException {
    if (isEnabled()) {
      var path = createTempDirectory(temporaryFolder);
      Files.writeString(path.resolve("moveToTrashDirectory"), "");

      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
      Assert.assertFalse(TRASH_BIN.moveToTrash(path));
    }
  }

  @Test
  public void moveToTrashSameDirectory() throws IOException {
    if (isEnabled()) {
      var path = createTempDirectory(temporaryFolder);
      Files.writeString(path.resolve("moveToTrashSameDirectory"), "");

      Assert.assertTrue(TRASH_BIN.moveToTrash(path));

      Files.createDirectory(path);
      Files.writeString(path.resolve("moveToTrashSameDirectory"), "");
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
    }
  }

  /**
   * Check if the test is enabled.
   *
   * <p>macOS and Windows trash bin implementation only works in Native Image.
   */
  private static boolean isEnabled() {
    return Platform.getOperatingSystem().isLinux();
  }

  private static Path createTempFile(TemporaryFolder temporaryFolder) throws IOException {
    return temporaryFolder.newFile().toPath();
  }

  private static Path createTempDirectory(TemporaryFolder temporaryFolder) throws IOException {
    return temporaryFolder.newFolder("TrashTest").toPath();
  }
}
