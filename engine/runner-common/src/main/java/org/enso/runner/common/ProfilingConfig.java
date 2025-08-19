package org.enso.runner.common;

import java.nio.file.Path;
import java.time.Duration;
import org.apache.commons.io.FilenameUtils;
import scala.Option;

/**
 * Application profiling configuration.
 *
 * @param profilingPath the path to the profiling output file
 * @param profilingTime limit the profiling duration, as an infinite profiling duration may cause
 *     out-of-memory errors.
 */
public record ProfilingConfig(Option<Path> profilingPath, Option<Duration> profilingTime) {
  private static final String EventsLogSuffix = "";
  private static final String EventsLogExtension = "log";

  /** Disabled profiling config. */
  public static ProfilingConfig none() {
    return new ProfilingConfig(Option.empty(), Option.empty());
  }

  /**
   * Creates the path to the runtime events log with the same name as `profilingPath` but with the
   * `.log` extension.
   *
   * @return the path to the runtime events log file
   */
  public Option<Path> profilingEventsLogPath() {
    return profilingPath.map(
        x ->
            ProfilingConfig.modifyPath(
                x, ProfilingConfig.EventsLogSuffix, ProfilingConfig.EventsLogExtension));
  }

  /**
   * Modify the path by adding a suffix and changing the file extension.
   *
   * @param path the path to modify
   * @param suffix the suffix to add
   * @param extension the new file extension
   * @return the modified path
   */
  private static Path modifyPath(Path path, String suffix, String extension) {
    var directory = path.getParent();
    var fileName = path.getFileName().toString();
    var fileExtension = FilenameUtils.getExtension(fileName);
    var modifiedFileName = fileName + suffix + "." + extension;
    if (!fileExtension.isEmpty()) {
      var fileNameWithoutExtension =
          (fileName.endsWith("." + fileExtension))
              ? fileName.substring(0, fileName.length() - fileExtension.length() - 1)
              : fileName;
      modifiedFileName = fileNameWithoutExtension + suffix + "." + extension;
    }

    if (directory != null) {
      return directory.resolve(modifiedFileName);
    } else {
      return Path.of(modifiedFileName);
    }
  }
}
