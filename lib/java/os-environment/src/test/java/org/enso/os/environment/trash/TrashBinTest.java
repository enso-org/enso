package org.enso.os.environment.trash;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class TrashBinTest {

  private static final TrashBin TRASH_BIN = TrashBin.getCurrent();

  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();

  @Test
  public void isSupported() {
    Assert.assertTrue(TRASH_BIN.isSupported());
  }

  @Test
  public void moveToTrashFile() throws IOException {
    var path = createTempFile(temporaryFolder);

    Assert.assertTrue(TRASH_BIN.moveToTrash(path));
    Assert.assertFalse(TRASH_BIN.moveToTrash(path));
  }

  @Test
  public void moveToTrashSameFile() throws IOException {
    var path = createTempFile(temporaryFolder);

    Assert.assertTrue(TRASH_BIN.moveToTrash(path));

    Files.writeString(path, "");
    Assert.assertTrue(TRASH_BIN.moveToTrash(path));
  }

  @Test
  public void moveToTrashDirectory() throws IOException {
    var path = createTempDirectory(temporaryFolder);
    Files.writeString(path.resolve("moveToTrashDirectory"), "");

    Assert.assertTrue(TRASH_BIN.moveToTrash(path));
    Assert.assertFalse(TRASH_BIN.moveToTrash(path));
  }

  @Test
  public void moveToTrashSameDirectory() throws IOException {
    var path = createTempDirectory(temporaryFolder);
    Files.writeString(path.resolve("moveToTrashSameDirectory"), "");

    Assert.assertTrue(TRASH_BIN.moveToTrash(path));

    Files.createDirectory(path);
    Files.writeString(path.resolve("moveToTrashSameDirectory"), "");
    Assert.assertTrue(TRASH_BIN.moveToTrash(path));
  }

  private static Path createTempFile(TemporaryFolder temporaryFolder) throws IOException {
    return temporaryFolder.newFile().toPath();
  }

  private static Path createTempDirectory(TemporaryFolder temporaryFolder) throws IOException {
    return temporaryFolder.newFolder("TrashTest").toPath();
  }
}
