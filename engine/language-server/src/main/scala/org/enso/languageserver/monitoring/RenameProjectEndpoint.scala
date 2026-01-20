package org.enso.languageserver.monitoring

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import org.enso.jsonrpc._
import org.enso.languageserver.refactoring.RefactoringApi
import org.enso.languageserver.requesthandler.refactoring.RenameProjectHandler

import java.util.UUID

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/** An HTTP endpoint that handles rename requests of the opened project.
  *
  * @param timeout the runtime request timeout
  * @param runtimeConnector the runtime connector
  * @param actorFactory the actor creation factory
  * @param ec the execution context
  */
class RenameProjectEndpoint(
  timeout: FiniteDuration,
  runtimeConnector: ActorRef,
  actorFactory: ActorRefFactory
)(implicit ec: ExecutionContext)
    extends Endpoint
    with LazyLogging {

  implicit private val askTimeout: Timeout = Timeout(timeout)

  implicit val renameProjectParamsDecoder
    : Decoder[RefactoringApi.RenameProject.Params] =
    deriveDecoder[RefactoringApi.RenameProject.Params]

  implicit val renameProjectParamsEncoder
    : Encoder[RefactoringApi.RenameProject.Params] =
    deriveEncoder[RefactoringApi.RenameProject.Params]

  override def route: Route =
    renameProject

  private val renameProject = {
    path("refactoring" / "renameProject") {
      post {
        entity(as[String]) { body =>
          if (ReadinessMonitor.isReady) {
            handleRenameProject(body)
          } else {
            complete(
              StatusCodes.InternalServerError,
              Json.obj("error" -> Json.fromString("Not initialized")).noSpaces
            )
          }
        }
      }
    }
  }

  private def handleRenameProject(body: String): Route = {
    parse(body).flatMap(_.as[RefactoringApi.RenameProject.Params]) match {
      case Right(params) =>
        val handler = actorFactory.actorOf(
          RenameProjectHandler.props(timeout, runtimeConnector),
          s"rename-project-handler-${UUID.randomUUID()}"
        )

        val requestId = Id.String(UUID.randomUUID().toString)
        val request   = Request(RefactoringApi.RenameProject, requestId, params)

        val future = (handler ? request).map {
          case ResponseResult(
                RefactoringApi.RenameProject,
                `requestId`,
                Unused
              ) =>
            StatusCodes.OK -> Json
              .obj("status" -> Json.fromString("success"))
              .noSpaces
          case ResponseError(Some(`requestId`), error) =>
            StatusCodes.BadRequest -> Json
              .obj("error" -> Json.fromString(error.message))
              .noSpaces
          case _ =>
            StatusCodes.InternalServerError -> Json
              .obj("error" -> Json.fromString("Unexpected response"))
              .noSpaces
        }

        onComplete(future) {
          case Success((status, responseBody)) =>
            complete(status, responseBody)
          case Failure(ex) =>
            logger.error("Failed to rename project", ex)
            complete(
              StatusCodes.InternalServerError,
              Json.obj("error" -> Json.fromString(ex.getMessage)).noSpaces
            )
        }

      case Left(error) =>
        logger.error(s"Failed to parse rename project request: $error")
        complete(
          StatusCodes.BadRequest,
          Json
            .obj(
              "error" -> Json.fromString(
                s"Invalid request format: ${error.getMessage}"
              )
            )
            .noSpaces
        )
    }
  }
}

object RenameProjectEndpoint {

  /** Create a new endpoint for renaming the opened project.
    *
    * @param timeout the runtime request timeout
    * @param runtimeConnector the runtime connector
    * @param actorFactory the actor creation factory
    * @param ec the execution context
    * @return an instance of [[RenameProjectEndpoint]]
    */
  def apply(
    timeout: FiniteDuration,
    runtimeConnector: ActorRef,
    actorFactory: ActorRefFactory
  )(implicit ec: ExecutionContext): RenameProjectEndpoint =
    new RenameProjectEndpoint(timeout, runtimeConnector, actorFactory)
}
