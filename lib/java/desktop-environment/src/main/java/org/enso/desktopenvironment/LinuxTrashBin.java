package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import org.apache.commons.io.FileUtils;

/**
 * The Linux trash implementing the <a
 * href="https://specifications.freedesktop.org/trash-spec/trashspec-1.0.html">FreeDesktop.org Trash
 * specification</a>.
 *
 * <p>A trash directory contains two subdirectories, named info and files. The files directory
 * contains the trashed files, and the info directory contains the corresponding trashinfo metadata
 * for each trashed entry in the files directory.
 */
final class LinuxTrashBin implements TrashBin {

  private static final String XDG_DATA_HOME = "XDG_DATA_HOME";
  private static final String PATH_TRASH = "Trash";
  private static final String PATH_FILES = "files";
  private static final String PATH_INFO = "info";

  private final LinuxDirectories directories = new LinuxDirectories();

  @Override
  public boolean isSupported() {
    var trashDir = detectTrashDirectory();
    return Files.isDirectory(trashDir.resolve(PATH_FILES))
        && Files.isDirectory(trashDir.resolve(PATH_INFO));
  }

  @Override
  public boolean moveToTrash(Path path) {
    var trashDir = detectTrashDirectory();

    if (Files.exists(path) && isSupported()) {
      try {
        var trashInfo = TrashInfo.create(trashDir.resolve(PATH_INFO), path);

        try {
          Files.move(
              path,
              trashDir.resolve(PATH_FILES).resolve(trashInfo.fileName),
              StandardCopyOption.ATOMIC_MOVE);
          return true;
        } catch (IOException e) {
          boolean isSuccessful;
          if (Files.isDirectory(path)) {
            isSuccessful =
                moveDirectoryToDirectory(path, trashDir.resolve(PATH_FILES), trashInfo.fileName);
          } else {
            isSuccessful =
                moveFileToDirectory(path, trashDir.resolve(PATH_FILES), trashInfo.fileName);
          }

          if (!isSuccessful) {
            FileUtils.deleteQuietly(trashInfo.path.toFile());
          }

          return isSuccessful;
        }
      } catch (IOException e) {
        return false;
      }

    } else {
      return false;
    }
  }

  private static boolean moveFileToDirectory(Path from, Path to, String fileName) {
    var source = from.toFile();
    var destination = to.resolve(fileName).toFile();

    try {
      FileUtils.copyFile(source, destination);
      FileUtils.delete(source);

      return true;
    } catch (IOException e) {
      FileUtils.deleteQuietly(destination);
      return false;
    }
  }

  private static boolean moveDirectoryToDirectory(Path from, Path to, String fileName) {
    var source = from.toFile();
    var destination = to.resolve(fileName).toFile();
    try {
      FileUtils.copyDirectory(source, destination);
      FileUtils.deleteDirectory(source);

      return true;
    } catch (IOException e) {
      FileUtils.deleteQuietly(destination);
      return false;
    }
  }

  /**
   * Detect the path to a home trash directory of the current user.
   *
   * <p>The home trash directory should be automatically created for any new user. If the directory
   * does not exist, it will be created.
   *
   * @return the path to the trash directory.
   */
  private Path detectTrashDirectory() {
    var xdgDataHomeOverride = System.getenv(XDG_DATA_HOME);
    var xdgDataHome =
        xdgDataHomeOverride == null
            ? directories.getUserHome().resolve(".local").resolve("share")
            : Path.of(xdgDataHomeOverride);

    var trashDir = xdgDataHome.resolve(PATH_TRASH);

    try {
      Files.createDirectories(trashDir.resolve(PATH_FILES));
    } catch (IOException ignored) {
    }

    try {
      Files.createDirectories(trashDir.resolve(PATH_INFO));
    } catch (IOException ignored) {
    }

    return trashDir;
  }

  /**
   * The trashinfo metadata file.
   *
   * @param path the path to this trashinfo file.
   * @param fileName the file name that should be used to store the trashed file.
   */
  private record TrashInfo(Path path, String fileName) {

    private static final int SUFFIX_SIZE = 6;
    private static final int MAX_ATTEMPTS = Byte.MAX_VALUE;
    private static final String TRASHINFO_EXTENSION = ".trashinfo";

    /**
     * Create the .trashinfo file containing the deleted file metadata.
     *
     * @param trashInfo the path to the trashinfo directory.
     * @param toDelete the path to the file that should be deleted.
     * @return the trashinfo metadata file.
     * @throws IOException if the file creation was unsuccessful.
     */
    public static TrashInfo create(Path trashInfo, Path toDelete) throws IOException {
      var builder = new StringBuilder();
      builder.append("[Trash Info]");
      builder.append(System.lineSeparator());
      builder.append("Path=");
      builder.append(toDelete.toAbsolutePath());
      builder.append(System.lineSeparator());
      builder.append("DeletionDate=");
      builder.append(
          LocalDateTime.now()
              .truncatedTo(ChronoUnit.SECONDS)
              .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
      builder.append(System.lineSeparator());

      return createTrashInfo(trashInfo, toDelete, builder, "", 0);
    }

    /**
     * Create the .trashinfo file containing the deleted file metadata.
     *
     * <p>In case of a name clash, when the trash already contains the file with the same name, the
     * trashinfo file is created with a random suffix to resolve the conflict.
     *
     * <p>The file creation is atomic to so that if two processes try trash files with the same
     * filename this will result in two different trash files.
     *
     * @param trashInfo the path to the trashinfo directory.
     * @param toDelete the path to the file that should be deleted.
     * @param contents the trashinfo file contents.
     * @param suffix the trashinfo suffix to resolve the file name conflicts.
     * @param attempts the number of attempts to resolve the name clash.
     * @return the trashinfo metadata file.
     * @throws IOException if the file creation was unsuccessful.
     */
    private static TrashInfo createTrashInfo(
        Path trashInfo, Path toDelete, CharSequence contents, String suffix, int attempts)
        throws IOException {
      if (attempts > MAX_ATTEMPTS) {
        throw new IOException("Failed to create trashinfo file. Max attempts reached.");
      }

      try {
        var fileName = toDelete.getFileName().toString() + suffix;
        var path =
            Files.writeString(
                trashInfo.resolve(fileName + TRASHINFO_EXTENSION),
                contents,
                StandardCharsets.UTF_8,
                StandardOpenOption.CREATE_NEW,
                StandardOpenOption.WRITE);

        return new TrashInfo(path, fileName);
      } catch (FileAlreadyExistsException e) {
        return createTrashInfo(
            trashInfo,
            toDelete,
            contents,
            RandomUtils.alphanumericString(SUFFIX_SIZE),
            attempts + 1);
      }
    }
  }
}
