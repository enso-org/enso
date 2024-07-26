package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Test;

public class TrashBinTest {

  private static final TrashBin TRASH_BIN = Platform.getOperatingSystem().getTrashBin();

  @Test
  public void isSupported() {
    Assert.assertTrue(TRASH_BIN.isSupported());
  }

  @Test
  public void moveToTrashFile() throws IOException {
    var path = writeTempFile("moveToTrashFile");

    try {
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
      Assert.assertFalse(TRASH_BIN.moveToTrash(path));
    } finally {
      FileUtils.deleteQuietly(path.toFile());
    }
  }

  @Test
  public void moveToTrashSameFile() throws IOException {
    var path = writeTempFile("moveToTrashSameFile");

    try {
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));

      Files.writeString(path, "moveToTrashSameFile2");
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
    } finally {
      FileUtils.deleteQuietly(path.toFile());
    }
  }

  @Test
  public void moveToTrashDirectory() throws IOException {
    var path = createTempDirectory();
    Files.writeString(path.resolve("moveToTrashDirectory"), "moveToTrashDirectory");

    try {
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
      Assert.assertFalse(TRASH_BIN.moveToTrash(path));
    } finally {
      FileUtils.deleteQuietly(path.toFile());
    }
  }

  @Test
  public void moveToTrashSameDirectory() throws IOException {
    var path = createTempDirectory();
    Files.writeString(path.resolve("moveToTrashSameDirectory"), "moveToTrashSameDirectory");

    try {
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));

      Files.createDirectory(path);
      Files.writeString(path.resolve("moveToTrashSameDirectory"), "moveToTrashSameDirectory2");
      Assert.assertTrue(TRASH_BIN.moveToTrash(path));
    } finally {
      FileUtils.deleteQuietly(path.toFile());
    }
  }

  private static Path writeTempFile(CharSequence contents) throws IOException {
    var path = Files.createTempFile("TrashTest", ".tmp");
    return Files.writeString(path, contents);
  }

  private static Path createTempDirectory() throws IOException {
    return Files.createTempDirectory("TrashTest");
  }
}
