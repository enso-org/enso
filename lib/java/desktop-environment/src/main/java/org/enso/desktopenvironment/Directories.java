package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Path;
import org.enso.common.Platform;

/** Provides information about user directories. */
public sealed interface Directories permits LinuxDirectories, MacOsDirectories, WindowsDirectories {
  Directories CURRENT = getForCurrentPlatform();

  private static Directories getForCurrentPlatform() {
    return switch (Platform.getOperatingSystem()) {
      case Platform.LINUX -> LinuxDirectories.getInstance();
      case Platform.MACOS -> MacOsDirectories.getInstance();
      case Platform.WINDOWS -> WindowsDirectories.getInstance();
    };
  }

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
