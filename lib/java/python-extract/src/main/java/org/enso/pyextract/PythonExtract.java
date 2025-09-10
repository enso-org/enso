package org.enso.pyextract;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Comparator;
import org.graalvm.polyglot.Engine;

/**
 * Extracts python resources from python graalvm component. Is basically equivalent to directly
 * extracting relevant files from the `python-resource.jar`.
 *
 * <p>Uses {@link Engine#copyResources(Path, String...)} for unpacking.
 */
public final class PythonExtract {
  public static void main(String[] args) throws IOException {
    if (args.length != 1) {
      throw new IllegalArgumentException("Usage: java -jar pyextract.jar <output-directory>");
    }
    var outDir = Path.of(args[0]);
    Files.createDirectories(outDir.getParent());
    unpackPythonResources(outDir);
  }

  /**
   * {@link Engine#copyResources(Path, String...)} copies not only python, but other component's
   * resources as well. We first copy it to a temporary directory, and from that temporary
   * directory, we copy only the `python` subdirectory.
   */
  private static void unpackPythonResources(Path outDir) throws IOException {
    var tmpDir = Files.createTempDirectory("py-resources");
    try {
      var copied = Engine.copyResources(tmpDir, "python");
      if (!copied) {
        throw new IllegalStateException("Failed to copy Python resources to " + outDir);
      }
      var pyDir = tmpDir.resolve("python");
      if (!Files.exists(pyDir)) {
        throw new IllegalStateException(
            "Python resources directory does not exist after copying: " + pyDir);
      }
      copyRecursively(outDir, pyDir);
    } finally {
      deleteRecursively(tmpDir);
    }
  }

  private static void deleteRecursively(Path tmpDir) {
    try {
      Files.walk(tmpDir)
          .sorted(Comparator.reverseOrder()) // delete files before directories
          .forEach(
              path -> {
                try {
                  Files.deleteIfExists(path);
                } catch (IOException e) {
                  System.err.println("Failed to delete " + path + ": " + e.getMessage());
                }
              });
    } catch (IOException e) {
      System.err.println("Failed to clean up temporary directory: " + e.getMessage());
    }
  }

  private static void copyRecursively(Path destDir, Path srcDir) {
    try {
      Files.walk(srcDir)
          .forEach(
              sourcePath -> {
                Path destPath = destDir.resolve(srcDir.relativize(sourcePath));
                try {
                  Files.copy(sourcePath, destPath, StandardCopyOption.REPLACE_EXISTING);
                } catch (IOException e) {
                  throw new RuntimeException(
                      "Failed to copy " + sourcePath + " to " + destPath + ": " + e.getMessage(),
                      e);
                }
              });
    } catch (IOException e) {
      throw new RuntimeException("Failed to walk source directory: " + e.getMessage(), e);
    }
  }
}
