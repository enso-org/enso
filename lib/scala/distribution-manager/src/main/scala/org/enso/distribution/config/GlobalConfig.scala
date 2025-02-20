package org.enso.distribution.config

import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node}

import java.util

/** Global user configuration.
  *
  * Specifies default values for new projects and possibly other properties. Can
  * handle unknown keys which can be used for configuration of plugins.
  *
  * @param defaultVersion default Enso version for launching Enso outside of
  *                       projects and creating new projects
  * @param authorName default author (and maintainer) name for newly created
  *                   projects
  * @param authorEmail default author (and maintainer) email for newly created
  *                    projects
  * @param editionProviders a sequence of edition provider URLs that are used
  *                         for downloading editions
  */
case class GlobalConfig(
  defaultVersion: DefaultVersion,
  authorName: Option[String],
  authorEmail: Option[String],
  editionProviders: Seq[String]
) {
  def findByKey(key: String): Option[String] = {
    key match {
      case GlobalConfig.Fields.DefaultVersion =>
        Option(defaultVersion).map(_.toString)
      case GlobalConfig.Fields.AuthorName =>
        authorName
      case GlobalConfig.Fields.AuthorEmail =>
        authorEmail
      case GlobalConfig.Fields.EditionProviders =>
        Option(editionProviders).map(_.toString())
      case _ =>
        None
    }
  }
}

object GlobalConfig {
  private val defaultEditionProviders: Seq[String] = Seq(
    "https://editions.release.enso.org/enso"
  )

  /** The default configuration used when the configuration file does not exist.
    */
  val Default: GlobalConfig =
    GlobalConfig(
      defaultVersion   = DefaultVersion.LatestInstalled,
      authorName       = None,
      authorEmail      = None,
      editionProviders = defaultEditionProviders
    )

  /** Field names used when serializing the configuration.
    */
  object Fields {
    val DefaultVersion   = "default.enso-version"
    val AuthorName       = "author.name"
    val AuthorEmail      = "author.email"
    val EditionProviders = "edition-providers"
  }

  implicit val yamlDecoder: YamlDecoder[GlobalConfig] =
    new YamlDecoder[GlobalConfig] {
      override def decode(node: Node) = node match {
        case node: MappingNode =>
          val bindings = mappingKV(node)
          val defaultVersionDecoder =
            implicitly[YamlDecoder[DefaultVersion]]
          val stringDecoder    = implicitly[YamlDecoder[String]]
          val seqStringDecoder = implicitly[YamlDecoder[Seq[String]]]

          val defaultVersionOpt = bindings.get("default") match {
            case Some(versionNode: MappingNode) =>
              val versionBindings = mappingKV(versionNode)
              versionBindings
                .get("enso-version")
                .toRight(
                  new YAMLException(s"missing '${Fields.DefaultVersion}' field")
                )
                .flatMap(defaultVersionDecoder.decode)
            case _ =>
              // Fallback
              bindings
                .get(Fields.DefaultVersion)
                .map(defaultVersionDecoder.decode)
                .getOrElse(Right(DefaultVersion.LatestInstalled))
          }
          val (nameOpt, emailOpt) = bindings.get("author") match {
            case Some(authorNode: MappingNode) =>
              val authorBindings = mappingKV(authorNode)
              (
                authorBindings
                  .get("name")
                  .map(stringDecoder.decode)
                  .getOrElse(Right(None))
                  .asInstanceOf[Either[Throwable, Option[String]]],
                authorBindings
                  .get("email")
                  .map(stringDecoder.decode)
                  .getOrElse(Right(None))
                  .asInstanceOf[Either[Throwable, Option[String]]]
              )
            case _ =>
              // Fallback
              (
                bindings
                  .get(Fields.AuthorName)
                  .map(stringDecoder.decode(_).map(Some(_)))
                  .getOrElse(Right(None)),
                bindings
                  .get(Fields.AuthorEmail)
                  .map(stringDecoder.decode(_).map(Some(_)))
                  .getOrElse(Right(None))
              )
          }
          val editionProviderOpt = bindings
            .get(Fields.EditionProviders)
            .map(seqStringDecoder.decode)
            .getOrElse(Right(Seq.empty))
          for {
            defaultVersion  <- defaultVersionOpt
            name            <- nameOpt
            email           <- emailOpt
            editionProvider <- editionProviderOpt
          } yield GlobalConfig(defaultVersion, name, email, editionProvider)
      }
    }

  implicit val yamlEncoder: YamlEncoder[GlobalConfig] =
    new YamlEncoder[GlobalConfig] {
      override def encode(value: GlobalConfig): AnyRef = {
        val defaultVersionEncoder = implicitly[YamlEncoder[DefaultVersion]]
        val editionProviders      = implicitly[YamlEncoder[Seq[String]]]
        val elements              = new util.ArrayList[(String, AnyRef)]()
        elements.add(
          (
            "default",
            toMap(
              "enso-version",
              defaultVersionEncoder.encode(value.defaultVersion)
            )
          )
        )
        if (value.authorName.nonEmpty || value.authorEmail.nonEmpty) {
          val authorElements = new util.ArrayList[(String, AnyRef)]()
          value.authorName.foreach(v => authorElements.add(("name", v)))
          value.authorEmail.foreach(v => authorElements.add(("email", v)))
          elements.add(("author", toMap(authorElements)))
        }
        if (value.editionProviders.nonEmpty) {
          elements.add(
            (
              Fields.EditionProviders,
              editionProviders.encode(value.editionProviders)
            )
          )
        }
        toMap(elements)
      }
    }
}
