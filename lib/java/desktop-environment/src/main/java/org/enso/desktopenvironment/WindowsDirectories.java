package org.enso.desktopenvironment;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

public class WindowsDirectories implements Directories {

  private static final String PROCESS_REG_QUERY =
      "reg query \"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\ShellFolders\" /v"
          + " personal";

  /**
   * Get the path to 'My Documents' user directory.
   *
   * <p>Method uses the registry query that may not work on Windows XP versions and below.
   *
   * @return the 'My Documents' user directory path.
   * @throws DirectoryException when fails to detect the user documents directory.
   */
  @Override
  public Path getDocuments() throws DirectoryException {
    try {
      var process = new ProcessBuilder(PROCESS_REG_QUERY).start();
      process.waitFor(3, TimeUnit.SECONDS);

      var baos = new ByteArrayOutputStream();
      var buffer = new byte[1024];
      try (var in = process.getInputStream()) {
        for (int length; (length = in.read(buffer)) != -1; ) {
          baos.write(buffer, 0, length);
        }
      }

      var stdoutString = baos.toString(StandardCharsets.UTF_8);
      var stdoutParts = stdoutString.split("\\s\\s+");
      if (stdoutParts.length < 5) {
        throw new DirectoryException(
            "Invalid Windows registry query output: '" + stdoutString + "'");
      }

      return Path.of(stdoutParts[4]);
    } catch (IOException e) {
      throw new DirectoryException("Failed to run Windows registry query", e);
    } catch (InterruptedException e) {
      throw new DirectoryException("Windows registry query timeout", e);
    }
  }
}
