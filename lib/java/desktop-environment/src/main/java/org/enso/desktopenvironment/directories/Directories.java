package org.enso.desktopenvironment.directories;

import java.nio.file.Path;

public interface Directories {

  default Path getUserHome() {
    return Path.of(System.getProperty("user.home"));
  }

  Path getDocuments() throws DirectoriesException;
}
