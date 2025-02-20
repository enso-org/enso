package org.enso.launcher.releases.launcher

import org.enso.launcher.releases.launcher
import org.enso.semver.SemVer
import org.enso.runtimeversionmanager.releases.ReleaseProviderException
import org.enso.scala.yaml.YamlDecoder
import org.enso.semver.SemVerYaml._
import org.enso.yaml.ParseError
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node}

import java.io.StringReader
import scala.util.{Failure, Try}

/** Contains release metadata associated with a launcher release.
  *
  * @param minimumVersionForUpgrade minimum version of the current launcher that
  *                                is required to upgrade to this version; if
  *                                current launcher is older than that provided
  *                                version, a multi-step upgrade must be
  *                                performed
  * @param filesToCopy a sequence of filenames of files that should be updated
  *                    in the data root
  * @param directoriesToCopy a sequence of names of directories that should be
  *                          updated in the data root
  */
case class LauncherManifest(
  minimumVersionForUpgrade: SemVer,
  filesToCopy: Seq[String],
  directoriesToCopy: Seq[String]
)

object LauncherManifest {

  /** Default name of the asset containing the launcher manifest.
    */
  val assetName: String = "launcher-manifest.yaml"

  private object Fields {
    val minimumVersionForUpgrade = "minimum-version-for-upgrade"
    val filesToCopy              = "files-to-copy"
    val directoriesToCopy        = "directories-to-copy"
  }

  implicit val yamlDecoder: YamlDecoder[LauncherManifest] =
    new YamlDecoder[LauncherManifest] {
      override def decode(node: Node): Either[Throwable, LauncherManifest] = {
        node match {
          case node: MappingNode =>
            val bindings         = mappingKV(node)
            val semverDecoder    = implicitly[YamlDecoder[SemVer]]
            val seqStringDecoder = implicitly[YamlDecoder[Seq[String]]]
            for {
              minimumVersionForUpgrade <- bindings
                .get(Fields.minimumVersionForUpgrade)
                .map(semverDecoder.decode)
                .getOrElse(
                  Left(
                    new YAMLException(
                      s"Required `${Fields.minimumVersionForUpgrade}` field is missing"
                    )
                  )
                )
              filesToCopy <- bindings
                .get(Fields.filesToCopy)
                .map(seqStringDecoder.decode)
                .getOrElse(Right(Seq.empty))
              directoriesToCopy <- bindings
                .get(Fields.directoriesToCopy)
                .map(seqStringDecoder.decode)
                .getOrElse(Right(Seq.empty))
            } yield LauncherManifest(
              minimumVersionForUpgrade,
              filesToCopy,
              directoriesToCopy
            )
        }
      }
    }

  /** Tries to parse the [[LauncherManifest]] from a [[String]].
    */
  def fromYAML(string: String): Try[LauncherManifest] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(new StringReader(string))).toEither
      .flatMap(
        implicitly[YamlDecoder[launcher.LauncherManifest]].decode(_)
      )
      .left
      .map(ParseError(_))
      .toTry
      .recoverWith { error =>
        // TODO [RW] more readable errors in #1111
        Failure(
          ReleaseProviderException(
            s"Cannot parse launcher manifest: $error.",
            error
          )
        )
      }
  }
}
