package org.enso.pkg.archive;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;

/** An enso project in .enso-project format. */
public record EnsoProjectArchive(byte[] bytes) {

  /**
   * Builds an enso-project archive.
   *
   * @param path a path to the enso project.
   * @return a project archive.
   */
  public static EnsoProjectArchive build(Path path) throws IOException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try (GzipCompressorOutputStream gzOut = new GzipCompressorOutputStream(out);
        TarArchiveOutputStream tarOut = new TarArchiveOutputStream(gzOut)) {
      tarOut.setLongFileMode(TarArchiveOutputStream.LONGFILE_POSIX);
      TarArchiveBuildingFileVisitor tarArchiveFileVisitor =
          new TarArchiveBuildingFileVisitor(path, tarOut);

      Files.walkFileTree(path, tarArchiveFileVisitor);
      tarOut.finish();
    }

    return new EnsoProjectArchive(out.toByteArray());
  }
}
