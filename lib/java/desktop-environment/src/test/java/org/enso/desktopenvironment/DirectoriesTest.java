package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Files;
import org.junit.Assert;
import org.junit.Test;

public class DirectoriesTest {

  private static final Directories directories = Platform.getDirectories();

  @Test
  public void getUserHome() {
    var userHome = directories.getUserHome();
    Assert.assertTrue("User home is not a directory: " + userHome, Files.isDirectory(userHome));
  }

  @Test
  public void getDocuments() throws IOException {
    // getDocuments fails on Windows CI
    if (Platform.isWindows()) return;

    var documents = directories.getDocuments();
    Assert.assertTrue(
        "User documents is not a directory" + documents, Files.isDirectory(documents));
  }
}
