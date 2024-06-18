package org.enso.desktopenvironment.directories;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

class WindowsDirectories implements Directories {

  private static final String[] PROCESS_REG_QUERY =
      new String[] {
        "reg",
        "query",
        "\"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\ShellFolders\"",
        "/v",
        "personal"
      };

  /**
   * Get the path to 'My Documents' user directory.
   *
   * <p>Method uses the registry query that may not work on Windows XP versions and below.
   *
   * @return the 'My Documents' user directory path.
   * @throws DirectoriesException when fails to detect the user documents directory.
   */
  @Override
  public Path getDocuments() throws DirectoriesException {
    try {
      var process = new ProcessBuilder(PROCESS_REG_QUERY).start();
      process.waitFor(3, TimeUnit.SECONDS);

      var stdoutString =
          new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
      var stdoutParts = stdoutString.split("\\s\\s+");
      if (stdoutParts.length < 5) {
        throw new DirectoriesException(
            "Invalid Windows registry query output: '" + stdoutString + "'");
      }

      return Path.of(stdoutParts[4].trim());
    } catch (IOException e) {
      throw new DirectoriesException("Failed to run Windows registry query", e);
    } catch (InterruptedException e) {
      throw new DirectoriesException("Windows registry query timeout", e);
    }
  }
}
