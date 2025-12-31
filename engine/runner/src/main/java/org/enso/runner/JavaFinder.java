package org.enso.runner;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.concurrent.TimeUnit;
import org.enso.common.Platform;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.Environment;
import org.enso.distribution.PortableDistributionManager;
import org.enso.runtimeversionmanager.components.GraalRuntime;
import org.enso.runtimeversionmanager.components.GraalVMVersion;
import org.enso.runtimeversionmanager.components.GraalVersionManager;
import org.enso.version.BuildVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Utility class that tries to find installed JDK on the system. */
final class JavaFinder {
  private static final Logger logger = LoggerFactory.getLogger(JavaFinder.class);

  private JavaFinder() {}

  /**
   * Tries to find {@code java} executable on the system. If a system-wide JDK is not found, tries
   * to find it in the {@link DistributionManager distribution} runtimes.
   *
   * @return null if cannot be found. Otherwise, returns the absolute path to the executable, or
   *     simply {@code java} if it is on the {@code PATH}.
   */
  static File findJavaExecutable() {
    var javaInRuntime = findJavaExecutableInDistributionRuntimes();
    if (javaInRuntime != null) {
      return javaInRuntime.toAbsolutePath().toFile();
    }
    logger.warn("No appropriate JDK found in the distribution runtimes. Trying system-wide JDK.");
    var javaHome = System.getenv("JAVA_HOME");
    if (javaHome != null) {
      var binDir = Path.of(javaHome).resolve("bin");
      Path javaExe;
      if (Platform.getOperatingSystem().isWindows()) {
        javaExe = binDir.resolve("java.exe");
      } else {
        javaExe = binDir.resolve("java");
      }
      if (javaExe.toFile().exists()) {
        logger.info("Found JDK in JAVA_HOME: {}", javaHome);
        return javaExe.toAbsolutePath().toFile();
      } else {
        logger.warn(
            "No JDK found in JAVA_HOME (missing Java executable at {}). Trying java on PATH.",
            javaExe);
      }
    } else {
      logger.warn("JAVA_HOME is not set. Trying java on PATH.");
    }

    if (findJavaOnPath() instanceof File javaExecutable) {
      logger.warn("Falling back to java on PATH: {}", javaExecutable);
      return javaExecutable;
    }
    logger.warn("No JDK found on PATH. Cannot start the runtime.");
    return null;
  }

  /**
   * Tries to find {@code java} executable in the distribution runtime with the same version that
   * was used for building, or a newer one.
   *
   * @return null if not found.
   */
  private static Path findJavaExecutableInDistributionRuntimes() {
    var env = new Environment() {};
    var distributionManager = new PortableDistributionManager(env);
    if (distributionManager.isRunningPortable()) {
      logger.trace("Running in portable distribution");
    }
    var graalVersionManager = new GraalVersionManager(distributionManager);
    var versionUsedForBuild =
        new GraalVMVersion(BuildVersion.graalVersion(), BuildVersion.javaVersion());
    var runtimeWithExactVersionMatch = graalVersionManager.findGraalRuntime(versionUsedForBuild);
    if (runtimeWithExactVersionMatch != null) {
      return runtimeWithExactVersionMatch.javaExecutable();
    }
    // Try to find newer runtime (JDK).
    var newerRuntime =
        graalVersionManager.getAllRuntimes().stream()
            .sorted(Comparator.comparing(GraalRuntime::version))
            .filter(runtime -> runtime.version().compareTo(versionUsedForBuild) > 0)
            .findFirst();
    if (newerRuntime.isPresent()) {
      logger.warn(
          "Found newer JDK [{}] than the one used for build [{}]",
          newerRuntime.get().version(),
          versionUsedForBuild);
      return newerRuntime.get().javaExecutable();
    }
    logger.trace(
        "No appropriate runtime found in the distribution. "
            + "graalVersionManager.getAllRuntimes() = {}, Paths of distributionManager = {}",
        graalVersionManager.getAllRuntimes(),
        distributionManager.paths());
    return null;
  }

  private static File findJavaOnPath() {
    try {
      ProcessBuilder processBuilder;
      if (Platform.getOperatingSystem().isWindows()) {
        processBuilder = new ProcessBuilder("java.exe", "-h");
      } else {
        processBuilder = new ProcessBuilder("java", "-h");
      }
      // don't pass -agentlib:jdwp & co. arguments to this process
      processBuilder.environment().remove("JAVA_TOOL_OPTIONS");
      Process process = processBuilder.start();
      var pathOpt = process.info().command();
      boolean exitSucc = process.waitFor(5L, TimeUnit.SECONDS);
      return exitSucc && pathOpt.isPresent() ? new File(pathOpt.get()) : null;
    } catch (IOException | InterruptedException e) {
      return null;
    }
  }
}
