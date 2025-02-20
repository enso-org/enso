package org.enso.runtimeversionmanager.components;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.Environment;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Utility class that finds installed managed runtimes (Graal JDK) from {@link DistributionManager}.
 */
public final class GraalVersionManager {
  private final DistributionManager distributionManager;
  private final Environment environment;
  private static final Logger logger = LoggerFactory.getLogger(GraalVersionManager.class);

  public GraalVersionManager(DistributionManager distributionManager, Environment environment) {
    this.distributionManager = distributionManager;
    this.environment = environment;
  }

  /**
   * Get all locally installed runtimes.
   *
   * @return Possibly empty list. Not null.
   */
  public List<GraalRuntime> getAllRuntimes() {
    var foundRuntimes = new ArrayList<GraalRuntime>();
    for (var runtimeSearchPath :
        CollectionConverters.asJava(distributionManager.paths().runtimeSearchPaths())) {
      if (runtimeSearchPath.toFile().isDirectory()) {
        var subdirs = runtimeSearchPath.toFile().listFiles();
        assert subdirs != null;
        for (var subdir : subdirs) {
          var parsedVersion = parseGraalRuntimeVersionString(subdir.getName());
          if (parsedVersion != null) {
            var foundRuntime = new GraalRuntime(parsedVersion, subdir.toPath());
            foundRuntime.ensureValid();
            foundRuntimes.add(foundRuntime);
          }
        }
      } else {
        logger.warn("Runtime search path `{}` is not a directory", runtimeSearchPath);
      }
    }
    return foundRuntimes;
  }

  /**
   * Tries to find a GraalVM runtime for the provided engine.
   *
   * <p>Returns null if the runtime is missing.
   */
  public GraalRuntime findGraalRuntime(Engine engine) {
    return findGraalRuntime(engine.manifest().runtimeVersion());
  }

  /**
   * Finds an installed GraalVM runtime with the given {@code version}.
   *
   * <p>Returns null if that version is not installed.
   */
  public GraalRuntime findGraalRuntime(GraalVMVersion version) {
    var explicitPathOpt = environment.getEnvPath("ENSO_JVM_PATH");
    if (explicitPathOpt.isDefined()) {
      var runtime = new GraalRuntime(version, explicitPathOpt.get());
      runtime.ensureValid();
      logger.debug("Found GraalVM runtime [{}]", runtime);
      return runtime;
    }
    var pathOpt = findGraalRuntimeOnSearchPath(version);
    if (pathOpt != null) {
      GraalRuntime runtime;
      try {
        runtime = loadGraalRuntime(pathOpt);
      } catch (Exception e) {
        throw new UnrecognizedComponentError(
            "The runtime "
                + version
                + "is already installed, but cannot be "
                + "loaded due to "
                + e.getMessage()
                + "."
                + "Until the launcher gets an auto-repair "
                + "feature, please try reinstalling the runtime by "
                + "uninstalling all engines that use it and installing them "
                + "again, or manually removing `"
                + pathOpt
                + "`",
            e);
      }
      logger.debug("Found GraalVM runtime [{}]", runtime);
      return runtime;
    }
    logger.debug("GraalVM runtime [{}] not found", version);
    return null;
  }

  public GraalRuntime loadGraalRuntime(Path path) throws UnrecognizedComponentError {
    logger.debug("Loading Graal runtime [{}]", path);
    var name = path.getFileName().toString();
    var version = parseGraalRuntimeVersionString(name);
    if (version == null) {
      throw new UnrecognizedComponentError("Invalid runtime component name `" + name + "`", null);
    }
    var runtime = new GraalRuntime(version, path);
    runtime.ensureValid();
    return runtime;
  }

  private Path findGraalRuntimeOnSearchPath(GraalVMVersion version) {
    var name = graalRuntimeNameForVersion(version);
    for (var runtimeSearchPath :
        CollectionConverters.asJava(distributionManager.paths().runtimeSearchPaths())) {
      var path = runtimeSearchPath.resolve(name);
      if (path.toFile().exists()) {
        return path;
      }
    }
    return null;
  }

  private GraalVMVersion parseGraalRuntimeVersionString(String name) {
    var pattern = Pattern.compile("graalvm-ce-java(.+)-(.+)");
    var matcher = pattern.matcher(name);
    if (matcher.matches()) {
      return new GraalVMVersion(matcher.group(2), matcher.group(1));
    }
    logger.warn("Unrecognized runtime name `{}`", name);
    return null;
  }

  private static String graalRuntimeNameForVersion(GraalVMVersion version) {
    return "graalvm-ce-java" + version.javaVersion() + "-" + version.graalVersion();
  }
}
