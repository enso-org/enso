package org.enso.languageserver.libraries

import io.circe.Json
import io.circe.literal.JsonStringContext
import org.enso.editions.LibraryName
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}

object LibraryApi {
  case object EditionsListAvailable extends Method("editions/listAvailable") {
    self =>

    // TODO [RW] Option or not?
    case class Params(update: Option[Boolean])

    case class Result(editionNames: Seq[String])

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object EditionsResolve extends Method("editions/resolve") {
    self =>

    case class Params(editionName: Option[String])

    case class Result(engineVersion: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object EditionsGetProjectSettings
      extends Method("editions/getProjectSettings") { self =>

    case class Result(
      parentEdition: Option[String],
      preferLocalLibraries: Boolean
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object EditionsSetParentEdition
      extends Method("editions/setParentEdition") { self =>

    case class Params(newEditionName: String)

    case class Result(needsRestart: Option[Boolean])

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object EditionsSetLocalLibrariesPreference
      extends Method("editions/setProjectLocalLibrariesPreference") { self =>

    case class Params(preferLocalLibraries: Boolean)

    case class Result(needsRestart: Option[Boolean])

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object EditionsListDefinedLibraries
      extends Method("editions/listDefinedLibraries") { self =>

    case class Params(editionName: Option[String])

    case class Result(availableLibraries: Seq[LibraryEntry])

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object LibraryListLocal extends Method("library/listLocal") { self =>

    case class Result(localLibraries: Seq[LibraryEntry])

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = self.Result
    }
  }

  case object LibraryCreate extends Method("library/create") { self =>

    case class Params(
      namespace: String,
      name: String,
      authors: Seq[String],
      maintainers: Seq[String],
      license: String
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object LibraryPublish extends Method("library/publish") { self =>

    case class Params(
      namespace: String,
      name: String,
      authToken: String,
      bumpVersionAfterPublish: Option[Boolean]
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object LibraryPreinstall extends Method("library/preinstall") { self =>

    case class Params(namespace: String, name: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = self.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case class EditionNotFoundError(editionName: String)
      extends Error(8001, s"Edition [$editionName] could not be found.") {
    override def payload: Option[Json] = Some(
      json""" { "editionName" : $editionName } """
    )
  }

  case class LibraryAlreadyExists(libraryName: LibraryName)
      extends Error(8002, s"Library [$libraryName] already exists.")

  case class LibraryRepositoryAuthenticationError(reason: String)
      extends Error(8003, s"Authentication failed: $reason.")

  case class LibraryPublishError(reason: String)
      extends Error(8004, s"Could not publish the library: $reason.")

  case class LibraryUploadError(reason: String)
      extends Error(8005, s"Could not upload the library: $reason.")

  case class LibraryDownloadError(reason: String)
      extends Error(8006, s"Could not download the library: $reason.")
}
