package org.enso.polyfill;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

public final class ClasspathResource {

  private static final String FILE_NAME_SEPARATOR = "-";

  private ClasspathResource() {}

  public static URI createTempFile(String name) throws IOException {
    var tempFileName = Path.of(name).getFileName().toString();
    var target = Files.createTempFile(null, FILE_NAME_SEPARATOR + tempFileName);
    target.toFile().deleteOnExit();

    try (InputStream in = ClasspathResource.class.getResourceAsStream(name)) {
      if (in == null) {
        throw new IOException("Cannot find " + name);
      }
      Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
    }

    return target.toUri();
  }
}
