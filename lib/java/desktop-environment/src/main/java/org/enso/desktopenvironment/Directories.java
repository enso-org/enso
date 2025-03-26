package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Path;

/** Provides information about user directories. */
public sealed interface Directories permits LinuxDirectories, MacOsDirectories, WindowsDirectories {

  /**
   * @return the user home directory.
   */
  default Path getUserHome() {
    return Path.of(System.getProperty("user.home"));
  }

  /**
   * @return the user documents directory.
   * @throws IOException when cannot detect the documents directory of the user.
   */
  Path getDocuments() throws IOException;
}
