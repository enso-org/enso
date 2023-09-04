package org.enso.pkg.archive;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.archivers.tar.TarConstants;
import org.enso.pkg.Package$;

/** A file visitor that builds a tar archive out of visited files. */
final class TarArchiveBuildingFileVisitor implements FileVisitor<Path> {

  private final Path root;
  private final TarArchiveOutputStream tarOut;

  public TarArchiveBuildingFileVisitor(Path root, TarArchiveOutputStream tarOut) {
    this.root = root;
    this.tarOut = tarOut;
  }

  @Override
  public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
    if (shouldSkipDirectory(dir)) {
      return FileVisitResult.SKIP_SUBTREE;
    }

    if (!root.equals(dir)) {
      try {
        TarArchiveEntry entry =
            new TarArchiveEntry(dir, root.relativize(dir).toString(), LinkOption.NOFOLLOW_LINKS);
        tarOut.putArchiveEntry(entry);
        closeArchiveEntry();
      } catch (CloseArchiveEntryException ex) {
        throw ex;
      } catch (IOException ignore) {
        // Failed to read directory. Skipping it
      }
    }

    return FileVisitResult.CONTINUE;
  }

  @Override
  public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
    try (InputStream fileIn =
        attrs.isSymbolicLink() ? InputStream.nullInputStream() : Files.newInputStream(file)) {
      TarArchiveEntry entry;

      if (attrs.isSymbolicLink()) {
        entry = new TarArchiveEntry(root.relativize(file).toString(), TarConstants.LF_SYMLINK);
        entry.setLinkName(Files.readSymbolicLink(file).toString());
      } else {
        entry = new TarArchiveEntry(file, root.relativize(file).toString());
      }

      tarOut.putArchiveEntry(entry);
      try {
        fileIn.transferTo(tarOut);
      } catch (IOException ex) {
        throw new WriteArchiveEntryException(ex);
      } finally {
        closeArchiveEntry();
      }
    } catch (EnsoProjectArchiveException ex) {
      throw ex;
    } catch (IOException ignore) {
      // File is not readable. Skipping it
    }

    return FileVisitResult.CONTINUE;
  }

  @Override
  public FileVisitResult visitFileFailed(Path file, IOException exc) {
    return FileVisitResult.CONTINUE;
  }

  @Override
  public FileVisitResult postVisitDirectory(Path dir, IOException exc) {
    return FileVisitResult.CONTINUE;
  }

  private boolean shouldSkipDirectory(Path dir) {
    return Package$.MODULE$.internalDirName().equals(dir.getFileName().toString());
  }

  private void closeArchiveEntry() throws IOException {
    try {
      tarOut.closeArchiveEntry();
    } catch (IOException ex) {
      // Failed to close archive Entry. Archive is invalid.
      throw new CloseArchiveEntryException(ex);
    }
  }
}
