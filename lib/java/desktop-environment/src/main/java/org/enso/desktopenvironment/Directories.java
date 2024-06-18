package org.enso.desktopenvironment;

import java.nio.file.Path;

public interface Directories {

  default Path getUserHome() {
    return Path.of(System.getProperty("user.home"));
  }

  Path getDocuments() throws DirectoryException;

  final class DirectoryException extends Exception {

    public DirectoryException(String message) {
      super(message);
    }

    public DirectoryException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
