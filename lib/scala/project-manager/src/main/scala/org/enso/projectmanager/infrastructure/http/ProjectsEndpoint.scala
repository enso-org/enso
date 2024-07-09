package org.enso.projectmanager.infrastructure.http

import akka.http.scaladsl.model.{
  ContentType,
  HttpEntity,
  MediaType,
  StatusCodes
}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.Endpoint
import org.enso.pkg.archive.EnsoProjectArchive
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.infrastructure.repository.{
  ProjectRepositoryFactory,
  ProjectRepositoryFailure
}

import java.io.File
import java.util.UUID
import scala.concurrent.Future
import scala.util.{Failure, Success}

final class ProjectsEndpoint[
  F[+_, +_]: Exec: CovariantFlatMap
](projectRepositoryFactory: ProjectRepositoryFactory[F])
    extends Endpoint
    with LazyLogging {

  /** @inheritdoc */
  override def route: Route =
    projectsEndpoint

  private val projectsEndpoint = {
    path("projects" / JavaUUID / "enso-project") { projectId =>
      parameters("projectsDirectory".optional) { directory =>
        get {
          getEnsoProject(projectId, directory)
        }
      }
    }
  }

  private def getEnsoProject(
    projectId: UUID,
    directory: Option[String]
  ): Route =
    onComplete(buildEnsoArchive(projectId, directory)) {
      case Failure(err) =>
        logger.error(
          "Failure when building an enso-project archive [{}].",
          projectId,
          err
        )
        complete(StatusCodes.InternalServerError)
      case Success(Left(err)) =>
        logger.error(
          "Failed to build an enso-project archive [{}]. {}",
          projectId,
          err
        )
        complete(StatusCodes.InternalServerError)
      case Success(Right(None)) =>
        complete(StatusCodes.NotFound)
      case Success(Right(Some(archive))) =>
        complete(
          HttpEntity(
            ProjectsEndpoint.CONTENT_TYPE_GZIP,
            archive.bytes()
          )
        )
    }

  private def buildEnsoArchive(
    projectId: UUID,
    projectsDirectory: Option[String]
  ): Future[Either[ProjectRepositoryFailure, Option[EnsoProjectArchive]]] =
    Exec[F].exec {
      projectRepositoryFactory
        .getProjectRepository(projectsDirectory.map(new File(_)))
        .findById(projectId)
        .map(projectOpt =>
          projectOpt.map(project =>
            EnsoProjectArchive.build(project.path.toPath)
          )
        )
    }
}

object ProjectsEndpoint {

  private val CONTENT_TYPE_GZIP =
    ContentType(
      MediaType.applicationBinary("gzip", MediaType.NotCompressible, "gz")
    )
}
