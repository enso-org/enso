package org.enso.desktopenvironment.directories;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

class LinuxDirectories implements Directories {

  private static final String[] PROCESS_XDG_DOCUMENTS = new String[] {"xdg-user-dir", "DOCUMENTS"};

  /**
   * Get the user 'Documents' directory.
   *
   * <p>Tries to obtain the documents directory from the XDG directory management system if
   * available and falls back to {@code $HOME/enso}.
   *
   * @return the path to the user documents directory.
   */
  @Override
  public Path getDocuments() {
    try {
      return getXdgDocuments();
    } catch (IOException | InterruptedException e) {
      return getUserHome().resolve("enso");
    }
  }

  private Path getXdgDocuments() throws IOException, InterruptedException {
    var process = new ProcessBuilder(PROCESS_XDG_DOCUMENTS).start();
    process.waitFor(3, TimeUnit.SECONDS);

    var documentsString =
        new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);

    return Path.of(documentsString.trim());
  }
}
