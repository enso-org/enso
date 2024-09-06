package org.enso.distribution.config

import org.enso.semver.SemVer
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.enso.semver.SemVerYaml._
import org.yaml.snakeyaml.nodes.{Node, ScalarNode}

/** Default version that is used when launching Enso outside of projects and
  * when creating new projects.
  */
sealed trait DefaultVersion
object DefaultVersion {

  /** Defaults to the latest installed version, or if no versions are installed,
    * to the latest available release.
    */
  case object LatestInstalled extends DefaultVersion {

    val name = "latest-installed"

    /** @inheritdoc
      */
    override def toString: String = name
  }

  /** Defaults to a specified version.
    */
  case class Exact(version: SemVer) extends DefaultVersion {

    /** @inheritdoc
      */
    override def toString: String = version.toString
  }

  implicit val yamlDecoder: YamlDecoder[DefaultVersion] =
    new YamlDecoder[DefaultVersion] {
      override def decode(node: Node) = {
        node match {
          case node if node == null =>
            Right(LatestInstalled)
          case scalarNode: ScalarNode =>
            scalarNode.getValue match {
              case LatestInstalled.name =>
                Right(LatestInstalled)
              case _ =>
                implicitly[YamlDecoder[SemVer]]
                  .decode(scalarNode)
                  .map(Exact(_))
            }
        }
      }
    }

  implicit val yamlEncoder: YamlEncoder[DefaultVersion] =
    new YamlEncoder[DefaultVersion] {
      override def encode(value: DefaultVersion): AnyRef = {
        value match {
          case latest @ LatestInstalled => latest.toString
          case Exact(version)           => version.toString
        }
      }
    }

  /** [[Argument]] instance for [[DefaultVersion]].
    */
  implicit val argument: Argument[DefaultVersion] = { string =>
    if (string == LatestInstalled.toString) Right(LatestInstalled)
    else {
      semverArgument.read(string).map(Exact)
    }
  }

  /** [[Argument]] instance that tries to parse the String as a [[SemVer]]
    * version string.
    */
  implicit val semverArgument: Argument[SemVer] = (string: String) =>
    SemVer
      .parse(string)
      .fold(
        _ =>
          Left(
            OptsParseError(s"`$string` is not a valid semantic version string.")
          ),
        v => Right(v)
      )
}
