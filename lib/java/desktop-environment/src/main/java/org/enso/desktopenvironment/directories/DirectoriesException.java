package org.enso.desktopenvironment.directories;

import java.io.IOException;

/** Indicates an issue when accessing user directories. */
public final class DirectoriesException extends IOException {

  public DirectoriesException(String message) {
    super(message);
  }

  public DirectoriesException(String message, Throwable cause) {
    super(message, cause);
  }
}
