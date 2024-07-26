package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Collectors;
import org.slf4j.Logger;

public class TrashBinFallback {

  protected boolean hardDeletePath(Path path, Logger logger) {
    var rootFile = path.toFile();
    if (rootFile.isDirectory()) {
      try {
        var deletedFiles =
            Files.walk(path)
                .sorted(Comparator.reverseOrder())
                .map(Path::toFile)
                .map(f -> new FileDeletion(f.getPath(), f.delete()))
                .filter(d -> !d.deleted())
                .collect(Collectors.toList());
        if (rootFile.exists()) {
          logger.error(
              "{} root directory failed to delete because of the following path(s):", rootFile);
          deletedFiles.forEach(d -> logger.error(" - {}", d.filePath()));
          return false;
        }
        return true;
      } catch (IOException e) {
        logger.error("Failed to hard delete path [{0}]", new Object[] {path, e});
        return false;
      }
    } else {
      return path.toFile().delete();
    }
  }

  private record FileDeletion(String filePath, boolean deleted) {}
}
