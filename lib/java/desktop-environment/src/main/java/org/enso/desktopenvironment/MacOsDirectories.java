package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Path;

final class MacOsDirectories implements Directories {

  private static final String DOCUMENTS = "Documents";

  /**
   * Get the user documents path.
   *
   * <p>On macOS, the 'Documents' directory acts like a symlink and points to the real
   * locale-dependent user documents folder.
   *
   * @return the path to the user documents directory.
   * @throws IOException when unable to resolve the real documents path.
   */
  @Override
  public Path getDocuments() throws IOException {
    try {
      return getUserHome().resolve(DOCUMENTS).toRealPath();
    } catch (IOException e) {
      throw new IOException("Failed to resolve real MacOs documents path", e);
    }
  }
}
