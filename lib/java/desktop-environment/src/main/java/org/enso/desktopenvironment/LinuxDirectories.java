package org.enso.desktopenvironment;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

public class LinuxDirectories implements Directories {

  private static final String PROCESS_XDG_DOCUMENTS = "xdg-user-dir DOCUMENTS";

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

    var baos = new ByteArrayOutputStream();
    var buffer = new byte[1024];
    try (var in = process.getInputStream()) {
      for (int length; (length = in.read(buffer)) != -1; ) {
        baos.write(buffer, 0, length);
      }
    }

    var documentsString = baos.toString(StandardCharsets.UTF_8);

    return Path.of(documentsString);
  }
}
