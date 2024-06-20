package org.enso.pkg

import io.circe._
import io.circe.syntax._
import io.circe.yaml.{Parser, Printer}
import org.enso.semver.SemVer
import org.enso.editions.EditionSerialization._
import org.enso.editions.{
  DefaultEnsoVersion,
  EditionName,
  Editions,
  EnsoVersion,
  SemVerEnsoVersion
}
import org.enso.pkg.validation.NameValidation
import org.enso.yaml.SnakeYamlDecoder
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node, ScalarNode}

import java.io.{Reader, StringReader}
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Try

/** Contact information to a user.
  *
  * Used for defining authors and maintainers.
  * At least one of the fields must not be None.
  *
  * @param name  contact name
  * @param email contact email
  */
case class Contact(name: Option[String], email: Option[String]) {
  if (name.isEmpty && email.isEmpty)
    throw new IllegalArgumentException(
      "At least one of fields `name` or `email` must be defined."
    )

  /** @inheritdoc */
  override def toString: String = {
    val space = if (name.isDefined && email.isDefined) " " else ""
    name.getOrElse("") + space + email.map(email => s"<$email>").getOrElse("")
  }
}

object Contact {

  /** Fields for use when serializing the [[Contact]]. */
  object Fields {
    val Name  = "name"
    val Email = "email"
  }

  /** [[Encoder]] instance for the [[Contact]]. */
  implicit val encoder: Encoder[Contact] = { contact =>
    val name  = contact.name.map(Fields.Name -> _.asJson)
    val email = contact.email.map(Fields.Email -> _.asJson)
    Json.obj((name.toSeq ++ email.toSeq): _*)
  }

  /** [[Decoder]] instance for the [[Contact]].
    */
  implicit val decoder: Decoder[Contact] = { json =>
    def verifyAtLeastOneDefined(
      name: Option[String],
      email: Option[String]
    ): Either[DecodingFailure, Unit] =
      if (name.isEmpty && email.isEmpty)
        Left(
          DecodingFailure(
            "At least one of the fields `name`, `email` must be defined.",
            json.history
          )
        )
      else Right(())

    for {
      name  <- json.getOrElse[Option[String]](Fields.Name)(None)
      email <- json.getOrElse[Option[String]](Fields.Email)(None)
      _     <- verifyAtLeastOneDefined(name, email)
    } yield Contact(name, email)
  }

  implicit val decoderSnake: SnakeYamlDecoder[Contact] =
    new SnakeYamlDecoder[Contact] {
      override def decode(node: Node): Either[Throwable, Contact] = node match {
        case mappingNode: MappingNode =>
          val optString = implicitly[SnakeYamlDecoder[Option[String]]]

          if (mappingNode.getValue.size() > 2)
            Left(new YAMLException("invalid number of fields for Contact"))
          else {
            val clazzMap = mappingNode.getValue.asScala.map { node =>
              node.getKeyNode match {
                case n: ScalarNode =>
                  (n.getValue, node.getValueNode)
                case _ =>
                  ???
              }
            }.toMap
            val result = for {
              name <- clazzMap
                .get("name")
                .map(optString.decode)
                .getOrElse(Right(None))
              email <- clazzMap
                .get("email")
                .map(optString.decode)
                .getOrElse(Right(None))
            } yield Contact(name, email)
            result
          }
      }
    }
}

/** Represents a package configuration stored in the `package.yaml` file.
  *
  * @param name the package display name
  * @param normalizedName the name that will be used as a prefix to module names
  *                       of the project
  * @param namespace package namespace.
  * @param version package version
  * @param license package license
  * @param authors name and contact information of the package author(s)
  * @param maintainers name and contact information of current package
  *                   maintainer(s)
  * @param edition the Edition associated with the project; it implies the
  *                engine version and dependency configuration to be used, if it
  *                is missing, the default edition should be used
  * @param preferLocalLibraries specifies if library resolution should prefer
  *                             local libraries over what is defined in the
  *                             edition
  * @param componentGroups the description of component groups provided by this
  *                        package
  * @param originalJson a Json object holding the original values that this
  *                     Config was created from, used to preserve configuration
  *                     keys that are not known
  */
case class Config(
  name: String,
  normalizedName: Option[String],
  namespace: String,
  version: String,
  license: String,
  authors: List[Contact],
  maintainers: List[Contact],
  edition: Option[Editions.RawEdition],
  preferLocalLibraries: Boolean,
  componentGroups: Option[ComponentGroups]
) {

  /** Converts the configuration into a YAML representation. */
  def toYaml: String =
    Printer.spaces2.copy(preserveOrder = true).pretty(Config.encoder(this))

  /** @return the module of name. */
  def moduleName: String =
    normalizedName.getOrElse(NameValidation.normalizeName(name))

}

object Config {

  val defaultNamespace: String    = "local"
  val defaultVersion: String      = "dev"
  val defaultLicense: String      = ""
  val defaultPreferLocalLibraries = false

  private object JsonFields {
    val name: String           = "name"
    val normalizedName: String = "normalized-name"
    val version: String        = "version"
    val ensoVersion: String    = "enso-version"
    val license: String        = "license"
    val author: String         = "authors"
    val namespace: String      = "namespace"
    val maintainer: String     = "maintainers"
    val edition: String        = "edition"
    val preferLocalLibraries   = "prefer-local-libraries"
    val componentGroups        = "component-groups"
  }

  implicit val yamlDecoder: SnakeYamlDecoder[Config] =
    new SnakeYamlDecoder[Config] {
      override def decode(node: Node): Either[Throwable, Config] = node match {
        case mappingNode: MappingNode =>
          if (mappingNode.getValue.size() > 10)
            Left(new YAMLException("invalid number of fields for Contact"))
          else {
            val clazzMap      = mappingKV(mappingNode)
            val stringDecoder = implicitly[SnakeYamlDecoder[String]]
            val normalizedNameDecoder =
              implicitly[SnakeYamlDecoder[Option[String]]]
            val contactDecoder     = implicitly[SnakeYamlDecoder[List[Contact]]]
            val editionNameDecoder = implicitly[SnakeYamlDecoder[EditionName]]
            val editionDecoder =
              implicitly[SnakeYamlDecoder[Option[Editions.RawEdition]]]
            val booleanDecoder = implicitly[SnakeYamlDecoder[Boolean]]
            val componentGroups =
              implicitly[SnakeYamlDecoder[Option[ComponentGroups]]]
            for {
              name <- clazzMap
                .get(JsonFields.name)
                .toRight(
                  new YAMLException(s"Missing '${JsonFields.name}' field")
                )
                .flatMap(stringDecoder.decode)
              normalizedName <- clazzMap
                .get(JsonFields.normalizedName)
                .map(normalizedNameDecoder.decode)
                .getOrElse(Right(None))
              namespace <- clazzMap
                .get(JsonFields.namespace)
                .map(stringDecoder.decode)
                .getOrElse(Right(defaultNamespace))
              version <- clazzMap
                .get(JsonFields.version)
                .map(stringDecoder.decode)
                .getOrElse(Right(defaultVersion))
              license <- clazzMap
                .get(JsonFields.license)
                .map(stringDecoder.decode)
                .getOrElse(Right(defaultLicense))
              authors <- clazzMap
                .get(JsonFields.author)
                .map(contactDecoder.decode)
                .getOrElse(Right(Nil))
              maintainers <- clazzMap
                .get(JsonFields.maintainer)
                .map(contactDecoder.decode)
                .getOrElse(Right(Nil))
              rawEdition = clazzMap
                .get(JsonFields.edition)
                .flatMap(x =>
                  editionNameDecoder.decode(x).toOption.map(Left(_))
                )
                .getOrElse(
                  clazzMap
                    .get(JsonFields.edition)
                    .map(editionDecoder.decode)
                    .getOrElse(Right(None))
                )
                .asInstanceOf[Either[EditionName, Option[Editions.RawEdition]]]
              edition <- rawEdition.fold(
                editionName =>
                  Right(
                    Some(Editions.Raw.Edition(parent = Some(editionName.name)))
                  ),
                r => Right(r)
              )
              preferLocalLibraries <- clazzMap
                .get(JsonFields.preferLocalLibraries)
                .map(booleanDecoder.decode)
                .getOrElse(Right(defaultPreferLocalLibraries))
              componentGroups <- clazzMap
                .get(JsonFields.componentGroups)
                .map(componentGroups.decode)
                .getOrElse(Right(None))
            } yield Config(
              name,
              normalizedName,
              namespace,
              version,
              license,
              authors,
              maintainers,
              edition,
              preferLocalLibraries,
              componentGroups
            )
          }
      }
    }

  implicit val decoder: Decoder[Config] = { json =>
    for {
      name           <- json.get[String](JsonFields.name)
      normalizedName <- json.get[Option[String]](JsonFields.normalizedName)
      namespace <- json.getOrElse[String](JsonFields.namespace)(
        defaultNamespace
      )
      version     <- json.getOrElse[String](JsonFields.version)(defaultVersion)
      ensoVersion <- json.get[Option[EnsoVersion]](JsonFields.ensoVersion)
      rawEdition <- json
        .get[EditionName](JsonFields.edition)
        .map(x => Left(x.name))
        .orElse(
          json
            .get[Option[Editions.RawEdition]](JsonFields.edition)
            .map(Right(_))
        )
      edition = rawEdition.fold(
        editionName => Some(Editions.Raw.Edition(parent = Some(editionName))),
        identity
      )
      license    <- json.getOrElse(JsonFields.license)(defaultLicense)
      author     <- json.getOrElse[List[Contact]](JsonFields.author)(List())
      maintainer <- json.getOrElse[List[Contact]](JsonFields.maintainer)(List())
      preferLocal <-
        json.getOrElse[Boolean](JsonFields.preferLocalLibraries)(
          defaultPreferLocalLibraries
        )
      finalEdition <-
        editionOrVersionBackwardsCompatibility(edition, ensoVersion).left.map {
          error => DecodingFailure(error, json.history)
        }
      componentGroups <- json.getOrElse[Option[ComponentGroups]](
        JsonFields.componentGroups
      )(None)
    } yield {

      Config(
        name                 = name,
        normalizedName       = normalizedName,
        namespace            = namespace,
        version              = version,
        license              = license,
        authors              = author,
        maintainers          = maintainer,
        edition              = finalEdition,
        preferLocalLibraries = preferLocal,
        componentGroups      = componentGroups
      )
    }
  }

  implicit val encoder: Encoder[Config] = { config =>
    val edition = config.edition
      .map { edition =>
        if (edition.isDerivingWithoutOverrides) edition.parent.get.asJson
        else edition.asJson
      }
      .map(JsonFields.edition -> _)

    val componentGroups =
      Option.unless(
        config.componentGroups.isEmpty
      )(
        JsonFields.componentGroups -> config.componentGroups.asJson
      )

    val normalizedName = config.normalizedName.map(value =>
      JsonFields.normalizedName -> value.asJson
    )

    val overrides =
      Seq(JsonFields.name -> config.name.asJson) ++
      normalizedName.toSeq ++
      Seq(
        JsonFields.namespace  -> config.namespace.asJson,
        JsonFields.version    -> config.version.asJson,
        JsonFields.license    -> config.license.asJson,
        JsonFields.author     -> config.authors.asJson,
        JsonFields.maintainer -> config.maintainers.asJson
      ) ++ edition.toSeq ++ componentGroups.toSeq

    val preferLocalOverride =
      if (config.preferLocalLibraries)
        Seq(JsonFields.preferLocalLibraries -> true.asJson)
      else Seq()
    val overridesObject = JsonObject(
      overrides ++ preferLocalOverride: _*
    )

    overridesObject.asJson
  }

  /** Tries to parse the [[Config]] from a YAML string. */
  /*def fromYaml(yamlString: String): Try[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toTry
  }*/

  /** Tries to parse the [[Config]] directly from the Reader */
  def fromYaml(reader: Reader): Try[Config] = {
    Parser.default.parse(reader).flatMap(_.as[Config]).toTry
  }

  def fromYaml(yamlString: String): Try[Config] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(new StringReader(yamlString))).toEither
      .flatMap(implicitly[SnakeYamlDecoder[Config]].decode(_))
      .toTry
  }

  /** Creates a simple edition that just defines the provided engine version.
    *
    * A compatibility layer for migrating from just specifying the engine
    * version to the edition system.
    *
    * TODO [RW] once the edition is actually used for loading libraries, this
    * may need to be revisited, because an edition created in this way will not
    * have any libraries present which is highly undesirable. We may either
    * remove the compatibility layer and return errors for the old format or
    * need to use the latest/default edition or some hardcoded edition for
    * compatibility.
    */
  def makeCompatibilityEditionFromVersion(
    ensoVersion: SemVer
  ): Editions.RawEdition = Editions.Raw.Edition(
    parent        = None,
    engineVersion = Some(ensoVersion),
    repositories  = Map(),
    libraries     = Map()
  )

  /** A helper method that reconciles the old and new fields of the config
    * related to the edition.
    *
    * If an edition is present, it is just returned as-is. If the engine version
    * is specified, a special edition is created that specifies this particular
    * engine version and nothing else.
    *
    * If both fields are defined, an error is raised as the configuration may be
    * inconsistent - the `engine-version` field should only be present in old
    * configs and after migration to the edition format it should be removed.
    */
  private def editionOrVersionBackwardsCompatibility(
    edition: Option[Editions.RawEdition],
    ensoVersion: Option[EnsoVersion]
  ): Either[String, Option[Editions.RawEdition]] =
    (edition, ensoVersion) match {
      case (Some(_), Some(_)) =>
        Left(
          s"The deprecated `${JsonFields.ensoVersion}` should not be defined " +
          s"if the `${JsonFields.edition}` that replaces it is already defined."
        )
      case (Some(edition), _) =>
        Right(Some(edition))
      case (_, Some(SemVerEnsoVersion(version))) =>
        Right(Some(makeCompatibilityEditionFromVersion(version)))
      case (_, Some(DefaultEnsoVersion)) =>
        // If the `default` version is specified, we return None, so that later
        // on, it will fallback to the default edition.
        Right(None)
      case (None, None) =>
        Right(None)
    }
}
