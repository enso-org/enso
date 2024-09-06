package org.enso.pkg

import org.yaml.snakeyaml.nodes.Tag
import org.enso.semver.SemVer
import org.enso.editions.{EditionName, Editions}
import org.enso.pkg.validation.NameValidation
import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node}

import java.io.{Reader, StringReader}
import java.util
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

  implicit val decoderSnake: YamlDecoder[Contact] =
    new YamlDecoder[Contact] {
      override def decode(node: Node): Either[Throwable, Contact] = node match {
        case mappingNode: MappingNode =>
          val optString = implicitly[YamlDecoder[Option[String]]]
          val bindings  = mappingKV(mappingNode)
          for {
            name <- bindings
              .get(Fields.Name)
              .map(optString.decode)
              .getOrElse(Right(None))
            email <- bindings
              .get(Fields.Email)
              .map(optString.decode)
              .getOrElse(Right(None))
          } yield Contact(name, email)
      }
    }

  implicit val encoderSnake: YamlEncoder[Contact] =
    new YamlEncoder[Contact] {
      override def encode(value: Contact) = {
        val elements = new util.ArrayList[(String, Object)]()
        value.name
          .map((Fields.Name, _))
          .foreach(elements.add)
        value.email
          .map((Fields.Email, _))
          .foreach(elements.add)
        toMap(elements)
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
  def toYaml: String = {
    val node          = implicitly[YamlEncoder[Config]].encode(this)
    val dumperOptions = new DumperOptions()
    dumperOptions.setIndent(2)
    dumperOptions.setPrettyFlow(true)
    val yaml = new Yaml(dumperOptions)
    yaml.dumpAs(node, Tag.MAP, DumperOptions.FlowStyle.BLOCK)
  }

  /** @return the module of name. */
  def moduleName: String =
    normalizedName.getOrElse(NameValidation.normalizeName(name))

}

object Config {

  val DefaultNamespace: String    = "local"
  val DefaultVersion: String      = "dev"
  val DefaultLicense: String      = ""
  val DefaultPreferLocalLibraries = false

  private object JsonFields {
    val Name: String           = "name"
    val NormalizedName: String = "normalized-name"
    val Version: String        = "version"
    val EnsoVersion: String    = "enso-version"
    val License: String        = "license"
    val Author: String         = "authors"
    val Namespace: String      = "namespace"
    val Maintainer: String     = "maintainers"
    val Edition: String        = "edition"
    val PreferLocalLibraries   = "prefer-local-libraries"
    val ComponentGroups        = "component-groups"
  }

  implicit val yamlDecoder: YamlDecoder[Config] =
    new YamlDecoder[Config] {
      override def decode(node: Node): Either[Throwable, Config] = node match {
        case mappingNode: MappingNode =>
          val clazzMap      = mappingKV(mappingNode)
          val stringDecoder = implicitly[YamlDecoder[String]]
          val normalizedNameDecoder =
            implicitly[YamlDecoder[Option[String]]]
          val contactDecoder     = implicitly[YamlDecoder[List[Contact]]]
          val editionNameDecoder = implicitly[YamlDecoder[EditionName]]
          val editionDecoder =
            implicitly[YamlDecoder[Option[Editions.RawEdition]]]
          val booleanDecoder = implicitly[YamlDecoder[Boolean]]
          val componentGroups =
            implicitly[YamlDecoder[Option[ComponentGroups]]]
          for {
            name <- clazzMap
              .get(JsonFields.Name)
              .toRight(
                new YAMLException(s"Missing '${JsonFields.Name}' field")
              )
              .flatMap(stringDecoder.decode)
            normalizedName <- clazzMap
              .get(JsonFields.NormalizedName)
              .map(normalizedNameDecoder.decode)
              .getOrElse(Right(None))
            namespace <- clazzMap
              .get(JsonFields.Namespace)
              .map(stringDecoder.decode)
              .getOrElse(Right(DefaultNamespace))
            version <- clazzMap
              .get(JsonFields.Version)
              .map(stringDecoder.decode)
              .getOrElse(Right(DefaultVersion))
            license <- clazzMap
              .get(JsonFields.License)
              .map(stringDecoder.decode)
              .getOrElse(Right(DefaultLicense))
            authors <- clazzMap
              .get(JsonFields.Author)
              .map(contactDecoder.decode)
              .getOrElse(Right(Nil))
            maintainers <- clazzMap
              .get(JsonFields.Maintainer)
              .map(contactDecoder.decode)
              .getOrElse(Right(Nil))
            rawEdition = clazzMap
              .get(JsonFields.Edition)
              .flatMap(x => editionNameDecoder.decode(x).toOption.map(Left(_)))
              .getOrElse(
                clazzMap
                  .get(JsonFields.Edition)
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
              .get(JsonFields.PreferLocalLibraries)
              .map(booleanDecoder.decode)
              .getOrElse(Right(DefaultPreferLocalLibraries))
            componentGroups <- clazzMap
              .get(JsonFields.ComponentGroups)
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

  implicit val encoderSnake: YamlEncoder[Config] =
    new YamlEncoder[Config] {
      override def encode(value: Config) = {
        val contactsEncoder = implicitly[YamlEncoder[List[Contact]]]
        val editionEncoder  = implicitly[YamlEncoder[Editions.RawEdition]]
        val booleanEncoder  = implicitly[YamlEncoder[Boolean]]
        val componentGroupsEncoder =
          implicitly[YamlEncoder[ComponentGroups]]

        val elements = new util.ArrayList[(String, Object)]()
        elements.add((JsonFields.Name, value.name))
        value.normalizedName.foreach(v =>
          elements.add((JsonFields.NormalizedName, v))
        )
        elements.add((JsonFields.Namespace, value.namespace))
        if (value.version != DefaultVersion)
          elements.add(
            (JsonFields.Version, value.version)
          )
        if (value.license != DefaultLicense)
          elements.add(
            (JsonFields.License, value.license)
          )
        if (value.authors.nonEmpty) {
          elements.add(
            (JsonFields.Author, contactsEncoder.encode(value.authors))
          )
        }
        if (value.maintainers.nonEmpty) {
          elements.add(
            (JsonFields.Maintainer, contactsEncoder.encode(value.maintainers))
          )
        }

        value.edition.foreach { edition =>
          if (edition.isDerivingWithoutOverrides)
            elements.add((JsonFields.Edition, edition.parent.get))
          else
            elements.add((JsonFields.Edition, editionEncoder.encode(edition)))
        }
        if (value.preferLocalLibraries != DefaultPreferLocalLibraries)
          elements.add(
            (
              JsonFields.PreferLocalLibraries,
              booleanEncoder.encode(value.preferLocalLibraries)
            )
          )
        value.componentGroups.foreach(v =>
          elements.add(
            (JsonFields.ComponentGroups, componentGroupsEncoder.encode(v))
          )
        )

        toMap(elements)
      }
    }

  /** Tries to parse the [[Config]] directly from the Reader */
  def fromYaml(reader: Reader): Try[Config] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(reader)).toEither
      .flatMap(implicitly[YamlDecoder[Config]].decode(_))
      .toTry
  }

  def fromYaml(yamlString: String): Try[Config] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(new StringReader(yamlString))).toEither
      .flatMap(implicitly[YamlDecoder[Config]].decode(_))
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
}
