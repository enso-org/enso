package org.enso.editions

import cats.Show
import io.circe.syntax.EncoderOps
import io.circe._
import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.{Raw, Repository}
import org.enso.editions.SemVerJson._

import java.io.FileReader
import java.net.URL
import java.nio.file.Path
import scala.util.{Try, Using}

object EditionSerialization {
  def parseYamlString(yamlString: String): Try[Raw.Edition] =
    yaml.parser
      .parse(yamlString)
      .flatMap(_.as[Raw.Edition])
      .toTry

  def loadEdition(path: Path): Try[Raw.Edition] =
    Using(new FileReader(path.toFile)) { reader =>
      yaml.parser
        .parse(reader)
        .flatMap(_.as[Raw.Edition])
        .toTry
    }.flatten

  implicit val editionDecoder: Decoder[Raw.Edition] = { json =>
    for {
      parent        <- json.get[Option[EditionName]](Fields.Parent)
      engineVersion <- json.get[Option[SemVer]](Fields.EngineVersion)
      _ <-
        if (parent.isEmpty && engineVersion.isEmpty)
          Left(
            DecodingFailure(
              s"The edition must specify at least one of " +
              s"${Fields.EngineVersion} or ${Fields.Parent}.",
              json.history
            )
          )
        else Right(())
      preferLocalLibraries <- json.get[Option[Boolean]](
        Fields.PreferLocalLibraries
      )
      repositories <-
        json.getOrElse[Seq[Repository]](Fields.Repositories)(Seq())
      libraries <- json.getOrElse[Seq[Raw.Library]](Fields.Libraries)(Seq())
      res <- {
        val repositoryMap = Map.from(repositories.map(r => (r.name, r)))
        val libraryMap    = Map.from(libraries.map(l => (l.qualifiedName, l)))
        if (libraryMap.size != libraries.size)
          Left(
            DecodingFailure(
              "Names of libraries defined within a single edition file must be unique.",
              json.downField(Fields.Libraries).history
            )
          )
        else if (repositoryMap.size != repositories.size)
          Left(
            DecodingFailure(
              "Names of repositories defined within a single edition file must be unique.",
              json.downField(Fields.Libraries).history
            )
          )
        else
          Right(
            Raw.Edition(
              parent               = parent.map(_.name),
              engineVersion        = engineVersion,
              preferLocalLibraries = preferLocalLibraries,
              repositories         = repositoryMap,
              libraries            = libraryMap
            )
          )
      }
    } yield res
  }

  implicit val editionEncoder: Encoder[Raw.Edition] = { edition =>
    val parent = edition.parent.map { parent => Fields.Parent -> parent.asJson }
    val engineVersion = edition.engineVersion.map { version =>
      Fields.EngineVersion -> version.asJson
    }

    if (parent.isEmpty && engineVersion.isEmpty) {
      throw new IllegalArgumentException(
        "Internal error: An edition must specify at least the engine version or extends clause"
      )
    }

    val repositories =
      if (edition.repositories.isEmpty) None
      else Some(Fields.Repositories -> edition.repositories.values.asJson)
    val libraries =
      if (edition.libraries.isEmpty) None
      else Some(Fields.Libraries -> edition.libraries.values.asJson)
    Json.obj(
      parent.toSeq ++ engineVersion.toSeq ++ repositories.toSeq ++ libraries.toSeq: _*
    )
  }

  object Fields {
    val Name                 = "name"
    val Version              = "version"
    val Repository           = "repository"
    val Url                  = "url"
    val Parent               = "extends"
    val EngineVersion        = "engine-version"
    val Repositories         = "repositories"
    val Libraries            = "libraries"
    val LocalRepositoryName  = "local"
    val PreferLocalLibraries = "prefer-local-libraries"
  }

  case class EditionName(name: String)

  implicit private val editionNameDecoder: Decoder[EditionName] = { json =>
    json
      .as[String]
      .orElse(json.as[Int].map(_.toString))
      .orElse(json.as[Float].map(_.toString))
      .map(EditionName)
  }

  implicit private val libraryDecoder: Decoder[Raw.Library] = { json =>
    def makeLibrary(name: String, repository: String, version: Option[SemVer]) =
      if (repository == Fields.LocalRepositoryName)
        if (version.isDefined)
          Left(
            DecodingFailure(
              "Version field must not be set for libraries associated with the local repository.",
              json.history
            )
          )
        else Right(Raw.LocalLibrary(name))
      else {
        version match {
          case Some(versionValue) =>
            Right(Raw.PublishedLibrary(name, versionValue, repository))
          case None =>
            Left(
              DecodingFailure(
                "Version field is mandatory for non-local libraries.",
                json.history
              )
            )
        }
      }
    for {
      name       <- json.get[String](Fields.Name)
      repository <- json.get[String](Fields.Repository)
      version    <- json.get[Option[SemVer]](Fields.Version)
      res        <- makeLibrary(name, repository, version)
    } yield res
  }

  implicit private val libraryEncoder: Encoder[Raw.Library] = {
    case Raw.LocalLibrary(name) =>
      Json.obj(
        Fields.Name       -> name.asJson,
        Fields.Repository -> Fields.LocalRepositoryName.asJson
      )
    case Raw.PublishedLibrary(name, version, repository) =>
      Json.obj(
        Fields.Name       -> name.asJson,
        Fields.Version    -> version.asJson,
        Fields.Repository -> repository.asJson
      )
  }

  implicit private val urlEncoder: Encoder[URL] = { url => url.toString.asJson }

  implicit private val urlDecoder: Decoder[URL] = { json =>
    json.as[String].flatMap { str =>
      Try(new URL(str)).toEither.left.map(throwable =>
        DecodingFailure(
          s"Cannot parse an URL: ${throwable.getMessage}",
          json.history
        )
      )
    }
  }

  implicit private val repositoryEncoder: Encoder[Repository] = { repo =>
    Json.obj(
      Fields.Name -> repo.name.asJson,
      Fields.Url  -> repo.url.asJson
    )
  }

  implicit private val repositoryDecoder: Decoder[Repository] = { json =>
    val nameField = json.downField(Fields.Name)
    for {
      name <- nameField.as[String]
      url  <- json.get[URL](Fields.Url)
      res <-
        if (name == Fields.LocalRepositoryName)
          Left(
            DecodingFailure(
              s"A defined repository cannot be called `${Fields.LocalRepositoryName}` which is a reserved keyword.",
              nameField.history
            )
          )
        else Right(Repository(name, url))
    } yield res
  }

  case class EditionLoadingError(message: String, cause: Throwable)
      extends RuntimeException(message, cause) {

    /** @inheritdoc */
    override def toString: String = message
  }

  object EditionLoadingError {

    /** Creates a [[EditionLoadingError]] by wrapping another [[Throwable]].
      *
      * Special logic is used for [[io.circe.Error]] to display the error
      * summary in a human-readable way.
      */
    def fromThrowable(throwable: Throwable): EditionLoadingError =
      throwable match {
        case decodingError: io.circe.Error =>
          val errorMessage =
            implicitly[Show[io.circe.Error]].show(decodingError)
          EditionLoadingError(
            s"Could not parse the edition file: $errorMessage",
            decodingError
          )
        case other =>
          EditionLoadingError(s"Could not load the edition file: $other", other)
      }
  }
}
