package org.enso.os.environment.directories;

import java.io.IOException;
import java.nio.file.Path;
import org.enso.common.Platform;

/** Provides information about user directories. */
public sealed interface Directories permits LinuxDirectories, MacOsDirectories, WindowsDirectories {
  static Directories getCurrent() {
    return switch (Platform.getOperatingSystem()) {
      case LINUX -> LinuxDirectories.getInstance();
      case WINDOWS -> WindowsDirectories.getInstance();
      case MACOS -> MacOsDirectories.getInstance();
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
