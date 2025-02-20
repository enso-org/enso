package org.enso.editions

import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.enso.semver.{SemVer, SemVerYaml}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, ScalarNode}
import YamlDecoder.MapKeyField
import org.yaml.snakeyaml.error.YAMLException

import java.util

/** Defines the general edition structure.
  *
  * We split the data type into two categories: Raw and Resolved editions.
  *
  * Raw editions are directly parsed from a YAML structure and can be serialized
  * back to it. They are just an object model of the underlying YAML format.
  *
  * The raw edition may reference a parent edition or repositories by name.
  * The [[EditionResolver]] takes care of resolving a Raw edition into a
  * Resolved instance by loading and parsing any of its parents and replacing
  * the repository by-name references by actual references to the repository
  * instances.
  */
trait Editions {

  /** The type of nested editions.
    *
    * Raw editions will refer to parents by their name and the Resolved edition
    * will contain a reference to an actual object.
    */
  type NestedEditionType

  /** The type of repository reference in listed libraries.
    *
    * Libraries in Raw editions will refer to repositories by their name and the
    * Resolved variant will contain a reference to the actual repository object
    * loaded from the edition or one of its parents.
    */
  type LibraryRepositoryType

  /** The library description included in the edition. */
  sealed trait Library {

    /** The qualified name of the library.
      *
      * It should consist of a prefix followed by a dot an the library name, for
      * example `Prefix.Library_Name`.
      */
    def name: LibraryName
  }

  implicit def nestedEditionTypeDecoder:     YamlDecoder[NestedEditionType]
  implicit def libraryRepositoryTypeDecoder: YamlDecoder[LibraryRepositoryType]

  implicit def nestedEditionTypeEncoder:     YamlEncoder[NestedEditionType]
  implicit def libraryRepositoryTypeEncoder: YamlEncoder[LibraryRepositoryType]

  object Library {

    trait LibraryFields {
      val Name = "name"
    }

    object LocalLibraryFields extends LibraryFields
    object PublishedLibraryFields extends LibraryFields {
      val Version    = "version"
      val Repository = "repository"
    }

    implicit val yamlDecoder: YamlDecoder[Library] =
      new YamlDecoder[Library] {
        override def decode(node: Node): Either[Throwable, Library] =
          node match {
            case mappingNode: MappingNode =>
              val bindings = mappingKV(mappingNode)

              bindings.get(LocalLibraryFields.Name) match {
                case Some(node) =>
                  val repoField =
                    bindings.get(PublishedLibraryFields.Repository)

                  val isLocalRepo = repoField
                    .map(node =>
                      node.isInstanceOf[ScalarNode] && node
                        .asInstanceOf[ScalarNode]
                        .getValue == "local"
                    )
                    .getOrElse(false)
                  if (isLocalRepo) {
                    if (bindings.contains(PublishedLibraryFields.Version))
                      Left(
                        new YAMLException(
                          s"'${PublishedLibraryFields.Version}' field must not be set for libraries associated with the local repository"
                        )
                      )
                    else
                      implicitly[YamlDecoder[LibraryName]]
                        .decode(node)
                        .map(LocalLibrary(_))
                  } else {
                    val libraryNameDecoder =
                      implicitly[YamlDecoder[LibraryName]]
                    val versionDecoder = SemVerYaml.yamlSemverDecoder
                    val repositoryDecoder =
                      implicitly[YamlDecoder[LibraryRepositoryType]]
                    for {
                      name <- bindings
                        .get(PublishedLibraryFields.Name)
                        .toRight(
                          new YAMLException(
                            s"Missing '${PublishedLibraryFields.Name}' field"
                          )
                        )
                        .flatMap(libraryNameDecoder.decode)
                      version <- bindings
                        .get(PublishedLibraryFields.Version)
                        .toRight(
                          new YAMLException(
                            s"'${PublishedLibraryFields.Version}' field is mandatory for non-local libraries"
                          )
                        )
                        .flatMap(versionDecoder.decode)
                      repository <- bindings
                        .get(PublishedLibraryFields.Repository)
                        .toRight(
                          new YAMLException(
                            s"Missing '${PublishedLibraryFields.Repository}' field"
                          )
                        )
                        .flatMap(repositoryDecoder.decode)
                    } yield PublishedLibrary(name, version, repository)
                  }
                case None =>
                  Left(
                    new YAMLException(
                      s"Library requires `${LocalLibraryFields.Name}` field"
                    )
                  )
              }
          }
      }

    implicit val yamlEncoder: YamlEncoder[Library] =
      new YamlEncoder[Library] {
        import SemVerYaml._
        override def encode(value: Library) = {
          val libraryNamencoder = implicitly[YamlEncoder[LibraryName]]
          val elements          = new util.ArrayList[(String, Object)](1)
          value match {
            case local: LocalLibrary =>
              elements.add(
                (LocalLibraryFields.Name, libraryNamencoder.encode(local.name))
              )
            case remote: PublishedLibrary =>
              val semverEncoder = implicitly[YamlEncoder[SemVer]]
              val repoEncoder =
                implicitly[YamlEncoder[LibraryRepositoryType]]
              elements.add(
                (
                  PublishedLibraryFields.Name,
                  libraryNamencoder.encode(remote.name)
                )
              )
              elements.add(
                (
                  PublishedLibraryFields.Version,
                  semverEncoder.encode(remote.version)
                )
              )
              elements.add(
                (
                  PublishedLibraryFields.Repository,
                  repoEncoder.encode(remote.repository)
                )
              )
          }
          toMap(elements)
        }
      }
  }

  /** Represents a local library. */
  case class LocalLibrary(override val name: LibraryName) extends Library

  /** Represents a specific version of the library that is published in a
    * repository.
    *
    * @param name the qualified name of the library
    * @param version the exact version of the library that should be used
    * @param repository the recommended repository to download the library from,
    *                   if it is not yet cached
    */
  case class PublishedLibrary(
    override val name: LibraryName,
    version: SemVer,
    repository: LibraryRepositoryType
  ) extends Library

  /** An Edition describing the library resolution configuration.
    *
    * @param parent a parent edition (if applicable)
    * @param engineVersion an engine version; it should be defined if the
    *                      edition wants to override the setting from the parent
    *                      or if it has no parents
    * @param repositories a mapping of repositories directly defined in the
    *                     edition (does not include ones defined in the parents)
    * @param libraries a mapping of libraries directly defined in the edition
    *                  (does not include ones defined in the parents)
    */
  case class Edition(
    parent: Option[NestedEditionType]              = None,
    engineVersion: Option[SemVer]                  = None,
    repositories: Map[String, Editions.Repository] = Map.empty,
    libraries: Map[LibraryName, Library]           = Map.empty
  ) {
    if (parent.isEmpty && engineVersion.isEmpty)
      throw new IllegalArgumentException(
        "The edition must specify the engine version or a parent edition " +
        "that will imply it."
      )
  }

  object Edition {

    /** Alternative constructor for creating editions.
      *
      * Useful for manually created editions.
      *
      * @param parent a parent edition (if applicable)
      * @param engineVersion an engine version; it should be defined if the
      *                      edition wants to override the setting from the parent
      *                      or if it has no parents
      * @param repositories a list of repositories directly defined in the
      *                     edition (does not include ones defined in the parents)
      * @param libraries a list of libraries directly defined in the edition
      *                  (does not include ones defined in the parents)
      */
    def make(
      parent: Option[NestedEditionType]      = None,
      engineVersion: Option[SemVer]          = None,
      repositories: Seq[Editions.Repository] = Seq.empty,
      libraries: Seq[Library]                = Seq.empty
    ): Edition = Edition(
      parent,
      engineVersion,
      repositories.map(r => (r.name, r)).toMap,
      libraries.map(l => (l.name, l)).toMap
    )
  }

  object Fields {
    val Parent        = "extends"
    val EngineVersion = "engine-version"
    val Repositories  = "repositories"
    val Libraries     = "libraries"
  }

  implicit val yamlDecoder: YamlDecoder[Edition] =
    new YamlDecoder[Edition] {
      import org.enso.semver.SemVerYaml._
      override def decode(node: Node): Either[Throwable, Edition] = node match {
        case mappingNode: MappingNode =>
          val clazzMap = mappingKV(mappingNode)
          val parentDecoder =
            implicitly[YamlDecoder[Option[NestedEditionType]]]
          val semverDecoder   = implicitly[YamlDecoder[Option[SemVer]]]
          implicit val mapKey = MapKeyField.plainField("name")
          val repositoriesDecoder =
            implicitly[YamlDecoder[Map[String, Editions.Repository]]]
          val librariesDecoder =
            implicitly[YamlDecoder[Map[LibraryName, Library]]]
          for {
            parent <- clazzMap
              .get(Fields.Parent)
              .map(parentDecoder.decode)
              .getOrElse(Right(None))
            engineVersion <- clazzMap
              .get(Fields.EngineVersion)
              .map(semverDecoder.decode)
              .getOrElse(Right(None))
            _ <-
              if (parent.isEmpty && engineVersion.isEmpty)
                Left(
                  new YAMLException(
                    s"The edition must specify at least one of " +
                    s"`${Fields.EngineVersion}` or `${Fields.Parent}`"
                  )
                )
              else Right(())
            repositories <- clazzMap
              .get(Fields.Repositories)
              .map(repositoriesDecoder.decode)
              .getOrElse(Right(Map.empty[String, Editions.Repository]))
            libraries <- clazzMap
              .get(Fields.Libraries)
              .map(librariesDecoder.decode)
              .getOrElse(Right(Map.empty[LibraryName, Library]))

          } yield Edition(parent, engineVersion, repositories, libraries)
      }
    }

  implicit val yamlEncoder: YamlEncoder[Edition] =
    new YamlEncoder[Edition] {
      import SemVerYaml._
      override def encode(value: Edition) = {
        val parentEncoder = implicitly[YamlEncoder[NestedEditionType]]
        val semverEncoder = implicitly[YamlEncoder[SemVer]]
        val repositoriesEncoder =
          implicitly[YamlEncoder[Seq[Editions.Repository]]]
        val librariesEncoder = implicitly[YamlEncoder[Seq[Library]]]

        if (value.parent.isEmpty && value.engineVersion.isEmpty)
          throw new YAMLException(
            s"The edition must specify at least one of " +
            s"`${Fields.EngineVersion}` or `${Fields.Parent}`"
          )
        val elements = new util.ArrayList[(String, Object)]()
        value.parent
          .map(parentEncoder.encode)
          .foreach(n => elements.add((Fields.Parent, n)))
        value.engineVersion
          .map(semverEncoder.encode)
          .foreach(n => elements.add((Fields.EngineVersion, n)))
        if (value.libraries.nonEmpty)
          elements.add(
            (
              Fields.Repositories,
              repositoriesEncoder.encode(value.repositories.values.toSeq)
            )
          )
        if (value.repositories.nonEmpty)
          elements.add(
            (
              Fields.Libraries,
              librariesEncoder.encode(value.libraries.values.toSeq)
            )
          )
        toMap(elements)
      }
    }

}

object Editions {

  /** Represents a repository that provides libraries. */
  case class Repository(name: String, url: String)

  object Repository {

    object Fields {
      val Name = "name"
      val Url  = "url"
    }

    /** An alternative constructor for unnamed repositories.
      *
      * The URL is used as the repository name.
      */
    def apply(url: String): Repository = Repository(url, url)

    implicit val yamlDecoder: YamlDecoder[Repository] = {
      new YamlDecoder[Repository] {
        override def decode(node: Node): Either[Throwable, Repository] =
          node match {
            case mappingNode: MappingNode =>
              val stringDecoder = implicitly[YamlDecoder[String]]
              val clazzMap      = mappingKV(mappingNode)
              for {
                name <- clazzMap
                  .get(Fields.Name)
                  .toRight(
                    new YAMLException(s"Missing '${Fields.Name}' field")
                  )
                  .flatMap(stringDecoder.decode)
                url <- clazzMap
                  .get(Fields.Url)
                  .toRight(
                    new YAMLException(s"Missing '${Fields.Url}' field")
                  )
                  .flatMap(stringDecoder.decode)
              } yield Repository(name, url)
          }
      }
    }

    implicit val yamlEncoder: YamlEncoder[Repository] = {
      new YamlEncoder[Repository] {
        override def encode(value: Repository) = {
          val elements = new util.ArrayList[(String, Object)](2)
          elements.add((Fields.Name, value.name))
          elements.add((Fields.Url, value.url))
          toMap(elements)
        }
      }
    }

  }

  /** Implements the Raw editions that can be directly parsed from a YAML
    * configuration.
    *
    * All references are typed as String, because this is what is directly read
    * from the configuration, without any postprocessing.
    */
  object Raw extends Editions {
    override type NestedEditionType     = String
    override type LibraryRepositoryType = String

    implicit override def nestedEditionTypeDecoder
      : YamlDecoder[NestedEditionType] = YamlDecoder.stringDecoderYaml
    implicit override def libraryRepositoryTypeDecoder
      : YamlDecoder[LibraryRepositoryType] =
      YamlDecoder.stringDecoderYaml

    implicit override def nestedEditionTypeEncoder: YamlEncoder[String] =
      YamlEncoder.stringEncoderYaml

    implicit override def libraryRepositoryTypeEncoder: YamlEncoder[String] =
      YamlEncoder.stringEncoderYaml
  }

  /** Implements the Resolved editions which are obtained by analyzing the Raw
    * edition and loading any of its parents.
    */
  object Resolved extends Editions {
    override type NestedEditionType     = this.Edition
    override type LibraryRepositoryType = Repository

    implicit override def nestedEditionTypeDecoder
      : YamlDecoder[NestedEditionType] = yamlDecoder
    implicit override def libraryRepositoryTypeDecoder
      : YamlDecoder[Repository] = Repository.yamlDecoder

    implicit override def nestedEditionTypeEncoder
      : YamlEncoder[NestedEditionType] = yamlEncoder

    implicit override def libraryRepositoryTypeEncoder
      : YamlEncoder[Repository] = Repository.yamlEncoder
  }

  /** An alias for Raw editions. */
  type RawEdition = Raw.Edition

  /** An alias for Resolved editions. */
  type ResolvedEdition = Resolved.Edition

  /** Syntax helpers for resolved edition. */
  implicit class ResolvedEditionOps(edition: ResolvedEdition) {

    /** Resolves the engine version that is implied by this resolved edition. It
      * is either the version override directly specified in the edition or the
      * version implied by its parent.
      */
    def getEngineVersion: SemVer = edition.engineVersion.getOrElse {
      val parent = edition.parent.getOrElse {
        throw new IllegalStateException(
          "Internal error: Resolved edition does not imply an engine version."
        )
      }
      parent.getEngineVersion
    }

    /** Returns a mapping of all libraries defined in the edition, including any
      * libraries defined in parent editions (also taking into account the
      * overrides).
      */
    def getAllDefinedLibraries: Map[LibraryName, LibraryVersion] = {
      val parent =
        edition.parent.map(_.getAllDefinedLibraries).getOrElse(Map.empty)
      edition.libraries.foldLeft(parent) { case (map, (name, lib)) =>
        val version = lib match {
          case Resolved.LocalLibrary(_) => LibraryVersion.Local
          case Resolved.PublishedLibrary(_, version, repository) =>
            LibraryVersion.Published(version, repository)
        }
        map.updated(name, version)
      }
    }
  }

  /** Syntax helpers for a raw edition. */
  implicit class RawEditionOps(edition: RawEdition) {

    /** Checks if the edition configuration consists only of an `extends` field
      * and no other overrides.
      *
      * If this is the case, `package.yaml` can use the shortened edition field
      * syntax.
      */
    def isDerivingWithoutOverrides: Boolean =
      edition.parent.isDefined &&
      edition.engineVersion.isEmpty &&
      edition.libraries.isEmpty &&
      edition.repositories.isEmpty
  }
}
