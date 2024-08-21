package org.enso.runtimeversionmanager.components

import java.io.{FileReader, StringReader}
import java.nio.file.Path
import org.enso
import org.enso.semver.SemVer
import org.enso.cli.OS
import org.enso.semver.SemVerYaml._
import org.enso.runtimeversionmanager.components.Manifest.{
  JVMOption,
  RequiredInstallerVersions
}
import org.enso.runtimeversionmanager.components
import org.enso.scala.yaml.YamlDecoder
import org.enso.yaml.ParseError
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node}

import scala.util.{Failure, Try, Using}

/** Contains release metadata read from the manifest file that is attached to
  * each release.
  *
  * @param requiredInstallerVersions The minimum required version of the
  *                                  installer that ensures its runtime version
  *                                  management logic is compatible with this
  *                                  particular engine release. Earlier
  *                                  installer versions should not be able to
  *                                  download this release (as the logic for
  *                                  installing it has probably changed).
  *                                  Instead they should print a message that
  *                                  the installer should be upgraded to use
  *                                  that version.
  * @param graalVMVersion the version of the GraalVM runtime that has to be
  *                       used with this engine
  * @param graalJavaVersion the java version of that GraalVM runtime
  * @param jvmOptions a list of JVM options that should be added when running
  *                   this engine
  */
case class Manifest(
  requiredInstallerVersions: RequiredInstallerVersions,
  graalVMVersion: String,
  graalJavaVersion: String,
  jvmOptions: Seq[JVMOption],
  brokenMark: Boolean
) {
  require(GraalVMVersion.isCorrectVersionFormat(graalVMVersion))
  require(GraalVMVersion.isCorrectVersionFormat(graalJavaVersion))

  /** Returns a [[GraalVMVersion]] which encapsulates all version information
    * needed to find the runtime required for this release.
    */
  def runtimeVersion: GraalVMVersion =
    components.GraalVMVersion(graalVMVersion, graalJavaVersion)

  /** Returns the minimum required version of the installer that is required for
    * using this particular engine release.
    */
  def minimumRequiredVersion(implicit installerKind: InstallerKind): SemVer =
    installerKind match {
      case InstallerKind.Launcher =>
        requiredInstallerVersions.launcher
      case InstallerKind.ProjectManager =>
        requiredInstallerVersions.projectManager
    }
}

object Manifest {

  /** Stores minimum required versions of each installer kind described in the
    * [[Manifest]].
    */
  case class RequiredInstallerVersions(launcher: SemVer, projectManager: SemVer)

  object RequiredInstallerVersions {
    implicit val yamlDecoder: YamlDecoder[RequiredInstallerVersions] =
      new YamlDecoder[RequiredInstallerVersions] {
        override def decode(
          node: Node
        ): Either[Throwable, RequiredInstallerVersions] = {
          node match {
            case mappingNode: MappingNode =>
              val semverDecoder = implicitly[YamlDecoder[SemVer]]
              val bindings      = mappingKV(mappingNode)
              for {
                launcher <- bindings
                  .get(Manifest.Fields.MinimumLauncherVersion)
                  .toRight(
                    new YAMLException(
                      s"Missing `${Manifest.Fields.MinimumLauncherVersion}` field"
                    )
                  )
                  .flatMap(semverDecoder.decode)
                projectManager <- bindings
                  .get(Manifest.Fields.MinimumProjectManagerVersion)
                  .toRight(
                    new YAMLException(
                      s"Missing `${Manifest.Fields.MinimumProjectManagerVersion}` field"
                    )
                  )
                  .flatMap(semverDecoder.decode)
              } yield RequiredInstallerVersions(launcher, projectManager)
          }
        }
      }
  }

  /** Defines the name under which the manifest is included in the releases.
    */
  val DEFAULT_MANIFEST_NAME = "manifest.yaml"

  /** Context used to substitute context-dependent variables in an JVM option.
    *
    * The context depends on what engine is being run on the JVM.
    *
    * @param enginePackagePath absolute path to the engine that is being
    *                          launched
    */
  case class JVMOptionsContext(enginePackagePath: Path)

  /** Represents an option that is added to the JVM running an engine.
    *
    * @param value option value, possibly containing variables that will be
    *              substituted
    * @param osRestriction the option is added only on the specified operating
    *                      system, if it is None, it applies to all systems
    */
  case class JVMOption(value: String, osRestriction: Option[OS]) {

    /** Checks if the option applies on the operating system that is currently
      * running.
      */
    def isRelevant: Boolean =
      osRestriction.isEmpty || osRestriction.contains(OS.operatingSystem)

    /** Substitutes any variables based on the provided `context`.
      */
    def substitute(context: JVMOptionsContext): String =
      value.replace(
        "$enginePackagePath",
        context.enginePackagePath.toAbsolutePath.normalize.toString
      )
  }

  object JVMOption {
    private object Fields {
      val Os    = "os"
      val Value = "value"
    }

    implicit val yamlDecoder: YamlDecoder[JVMOption] =
      new YamlDecoder[JVMOption] {
        override def decode(node: Node): Either[Throwable, JVMOption] = {
          node match {
            case node: MappingNode =>
              val bindings      = mappingKV(node)
              val stringDecoder = implicitly[YamlDecoder[String]]
              val OSdecoder     = implicitly[YamlDecoder[OS]]
              for {
                value <- bindings
                  .get(Fields.Value)
                  .toRight(new YAMLException(s"missing `${Fields.Value} field"))
                  .flatMap(stringDecoder.decode)
                osRestriction <- bindings
                  .get(Fields.Os)
                  .map(OSdecoder.decode(_).map(Some(_)))
                  .getOrElse(Right(None))
              } yield JVMOption(value, osRestriction)
          }
        }
      }
  }

  /** Tries to load the manifest at the given path.
    *
    * Returns None if the manifest could not be opened or could not be parsed.
    */
  def load(path: Path): Try[Manifest] =
    Using(new FileReader(path.toFile)) { reader =>
      val snakeYaml = new org.yaml.snakeyaml.Yaml()
      Try(snakeYaml.compose(reader))
        .flatMap(
          implicitly[YamlDecoder[Manifest]].decode(_).toTry
        )
    }.flatten.recoverWith { error =>
      Failure(ManifestLoadingError.fromThrowable(error))
    }

  /** Parses the manifest from a string containing a YAML definition.
    *
    * Returns None if the definition cannot be parsed.
    */
  def fromYaml(yamlString: String): Try[Manifest] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(new StringReader(yamlString))).toEither
      .flatMap(implicitly[YamlDecoder[Manifest]].decode(_))
      .left
      .map(ParseError(_))
      .toTry
      .recoverWith { error =>
        Failure(ManifestLoadingError.fromThrowable(error))
      }
  }

  /** Indicates an error that prevented loading the engine manifest.
    */
  case class ManifestLoadingError(message: String, cause: Throwable)
      extends RuntimeException(message, cause) {

    /** @inheritdoc
      */
    override def toString: String = message
  }

  object ManifestLoadingError {

    /** Creates a [[ManifestLoadingError]] by wrapping another [[Throwable]].
      *
      * Special logic is used for [[io.circe.Error]] to display the error
      * summary in a human-readable way.
      */
    def fromThrowable(throwable: Throwable): ManifestLoadingError =
      throwable match {
        case other =>
          ManifestLoadingError(s"Could not load the manifest: $other", other)
      }
  }

  object Fields {
    val MinimumLauncherVersion       = "minimum-launcher-version"
    val MinimumProjectManagerVersion = "minimum-project-manager-version"
    val JvmOptions                   = "jvm-options"
    val GraalVMVersion               = "graal-vm-version"
    val GraalJavaVersion             = "graal-java-version"
    val NrokenMark                   = "broken"
  }

  implicit val yamlDecoder: YamlDecoder[Manifest] =
    new YamlDecoder[Manifest] {
      override def decode(node: Node): Either[Throwable, Manifest] = {
        node match {
          case node: MappingNode =>
            val bindings = mappingKV(node)
            val requiredInstallerVersionsDecoder =
              implicitly[YamlDecoder[RequiredInstallerVersions]]
            val stringDecoder = implicitly[YamlDecoder[String]]
            val seqJVMOptionsDecoder =
              implicitly[YamlDecoder[Seq[JVMOption]]]
            val booleanDecoder = implicitly[YamlDecoder[Boolean]]

            for {
              requiredInstallerVersions <- requiredInstallerVersionsDecoder
                .decode(node)
              graalVMVersion <- bindings
                .get(Fields.GraalVMVersion)
                .toRight(
                  new YAMLException(
                    s"Required `${Fields.GraalVMVersion}`field is missing"
                  )
                )
                .flatMap(stringDecoder.decode)
              graalJavaVersion <- bindings
                .get(Fields.GraalJavaVersion)
                .toRight(
                  new YAMLException(
                    s"Required `${Fields.GraalJavaVersion}` field is missing"
                  )
                )
                .flatMap(stringDecoder.decode)
              jvmOptions <- bindings
                .get(Fields.JvmOptions)
                .map(seqJVMOptionsDecoder.decode)
                .getOrElse(Right(Seq.empty))
              brokenMark <- bindings
                .get(Fields.NrokenMark)
                .map(booleanDecoder.decode)
                .getOrElse(Right(false))
            } yield Manifest(
              requiredInstallerVersions,
              graalVMVersion,
              graalJavaVersion,
              jvmOptions,
              brokenMark
            )
        }
      }
    }
}
