package org.enso.desktopenvironment;

import java.nio.file.Files;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class DirectoriesTest {

  private static Directories directories;

  @BeforeClass
  public static void setup() {
    directories = Platform.getDirectories();
  }

  @Test
  public void getUserHome() {
    var userHome = directories.getUserHome();
    Assert.assertTrue("User home is not a directory: " + userHome, Files.isDirectory(userHome));
  }

  @Test
  public void getDocuments() throws Directories.DirectoryException {
    var documents = directories.getDocuments();
    Assert.assertTrue(
        "User documents is not a directory" + documents, Files.isDirectory(documents));
  }
}
