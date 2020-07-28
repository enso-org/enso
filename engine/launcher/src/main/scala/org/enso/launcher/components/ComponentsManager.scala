package org.enso.launcher.components

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.{FileSystem, Launcher, Logger}
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.FileSystem.PathSyntax

case class Runtime(version: RuntimeVersion, path: Path) {
  override def toString: String =
    s"GraalVM ${version.graal}-java${version.java}"
}
case class Engine(version: SemVer, path: Path, manifest: Manifest)

class ComponentsManager(distributionManager: DistributionManager) {
  def listInstalledEngines(): Seq[Engine] =
    FileSystem
      .listDirectory(distributionManager.paths.engines)
      .flatMap(parseEngine)

  def listInstalledRuntimes(): Seq[Runtime] =
    FileSystem
      .listDirectory(distributionManager.paths.runtimes)
      .flatMap(parseGraalRuntime)

  def findRuntime(engine: Engine): Option[Runtime] = {
    val name = runtimeNameForVersion(engine.manifest.runtimeVersion)
    val path = distributionManager.paths.runtimes / name
    parseGraalRuntime(path)
  }

  def runtimeNameForVersion(version: RuntimeVersion): String =
    s"graalvm-ce-java${version.java}-${version.graal}"

  def findEnginesUsingRuntime(runtime: Runtime): Seq[Engine] =
    listInstalledEngines().filter(_.manifest.runtimeVersion == runtime.version)

  private def parseEngine(path: Path): Option[Engine] =
    for {
      version  <- parseEngineVersion(path)
      manifest <- loadEngineManifest(path)
    } yield Engine(version, path, manifest)

  private def parseEngineVersion(path: Path): Option[SemVer] = {
    val version = SemVer(path.getFileName.toString)
    if (version.isEmpty) {
      Logger.warn(s"Invalid engine component version `${path.getFileName}`.")
    }
    version
  }

  private def loadEngineManifest(path: Path): Option[Manifest] = {
    val manifest = Manifest.load(path / Manifest.DEFAULT_MANIFEST_NAME)
    manifest match {
      case Some(manifest) =>
        if (manifest.minimumLauncherVersion > Launcher.version) {
          Logger.warn(
            s"Engine `${path.getFileName}` requires launcher version >= " +
            s"${manifest.minimumLauncherVersion}, but the launcher is " +
            s"running ${Launcher.version}. This engine version will not be " +
            s"available until you upgrade the launcher."
          )
          None
        } else Some(manifest)
      case None =>
        Logger.warn(s"Cannot load manifest for `${path.getFileName}`.")
        None
    }
  }

  private def parseGraalRuntime(path: Path): Option[Runtime] = {
    val regex = """graalvm-ce-java(\d+)-(.+)""".r
    val name  = path.getFileName.toString
    name match {
      case regex(javaVersionString, graalVersionString) =>
        SemVer(graalVersionString) match {
          case Some(graalVersion) =>
            Some(Runtime(RuntimeVersion(graalVersion, javaVersionString), path))
          case None =>
            Logger.warn(
              s"Invalid runtime version string `$graalVersionString`."
            )
            None
        }
      case _ =>
        Logger.warn(
          s"Unrecognized runtime name `$name`."
        )
        None
    }
  }
}

object ComponentsManager extends ComponentsManager(DistributionManager)
