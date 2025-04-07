package org.enso.os.environment.directories;

import static org.junit.Assume.assumeFalse;

import java.io.IOException;
import java.nio.file.Files;
import org.enso.common.Platform;
import org.junit.Assert;
import org.junit.Test;

public class DirectoriesTest {

  private static final Directories directories = Directories.getCurrent();

  @Test
  public void getUserHome() {
    var userHome = directories.getUserHome();
    Assert.assertTrue("User home is not a directory: " + userHome, Files.isDirectory(userHome));
  }

  @Test
  public void getDocuments() throws IOException {
    // getDocuments fails on Windows CI
    ensureNotInWindows();

    var documents = directories.getDocuments();
    Assert.assertTrue(
        "User documents is not a directory" + documents, Files.isDirectory(documents));
  }

  private static void ensureNotInWindows() {
    assumeFalse(Platform.getOperatingSystem().isWindows());
  }
}
