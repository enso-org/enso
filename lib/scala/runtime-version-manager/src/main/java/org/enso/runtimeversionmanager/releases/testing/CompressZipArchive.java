package org.enso.runtimeversionmanager.releases.testing;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

class CompressZipArchive {
  static void compress(Path source, Path destination) throws IOException {
    try (var zip = Files.newOutputStream(destination);
        var zos = new ZipOutputStream(zip);
        var files = Files.walk(source)) {
      files
          .filter(path -> !Files.isDirectory(path))
          .forEach(
              path -> {
                var zipEntry = new ZipEntry(source.relativize(path).toString());
                try {
                  zos.putNextEntry(zipEntry);
                  Files.copy(path, zos);
                  zos.closeEntry();
                } catch (IOException e) {
                  throw new UncheckedIOException(e);
                }
              });
    }
  }
}
