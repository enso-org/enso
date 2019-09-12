package org.enso.projectmanager

import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Scheduler}
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.server.{Directives, Route}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.enso.projectmanager.api.{ProjectFactory, ProjectJsonSupport}
import org.enso.projectmanager.model.{Project, ProjectId}
import org.enso.projectmanager.services._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

case class Server(
  host: String,
  port: Int,
  repository: ActorRef[ProjectsCommand],
  routeHelper: RouteHelper,
  apiFactory: ProjectFactory
)(implicit val system: ActorSystem,
  implicit val executor: ExecutionContext,
  implicit val materializer: ActorMaterializer,
  implicit val askTimeout: Timeout)
    extends Directives
    with ProjectJsonSupport {

  implicit val scheduler: Scheduler = system.scheduler

  def projectDoesNotExistResponse(id: ProjectId): HttpResponse =
    HttpResponse(StatusCodes.NotFound, entity = s"Project $id does not exist")

  def thumbDoesNotExistResponse: HttpResponse =
    HttpResponse(StatusCodes.NotFound, entity = "Thumbnail does not exist")

  def withSuccess[T](
    fut: Future[T],
    errorResponse: HttpResponse = HttpResponse(StatusCodes.InternalServerError)
  )(successHandler: T => Route
  ): Route = {
    onComplete(fut) {
      case Success(r) => successHandler(r)
      case Failure(_) => complete(errorResponse)
    }
  }

  def withProject(id: ProjectId)(route: Project => Route): Route = {
    val projectFuture =
      repository
        .ask(
          (ref: ActorRef[GetProjectResponse]) => GetProjectById(id, ref)
        )
        .map(_.project)
    withSuccess(projectFuture) {
      case Some(project) => route(project)
      case None          => complete(projectDoesNotExistResponse(id))
    }
  }

  def listProjectsWith(
    reqBuilder: ActorRef[ListProjectsResponse] => ProjectsCommand
  )(baseUri: Uri
  ): Route = {
    val projectsFuture = repository.ask(reqBuilder)
    withSuccess(projectsFuture) { projectsResponse =>
      val response = projectsResponse.projects.toSeq.map {
        case (id, project) => apiFactory.fromModel(id, project, baseUri)
      }
      complete(response)
    }
  }

  def createProject(baseUri: Uri): Route = {
    val projectFuture = repository.ask(
      (ref: ActorRef[CreateTemporaryResponse]) =>
        CreateTemporary("NewProject", ref)
    )
    withSuccess(projectFuture) { response =>
      complete(apiFactory.fromModel(response.id, response.project, baseUri))
    }
  }

  def getThumb(projectId: ProjectId): Route = {
    withProject(projectId) { project =>
      if (project.pkg.hasThumb) getFromFile(project.pkg.thumbFile)
      else complete(thumbDoesNotExistResponse)
    }
  }

  val route: Route = ignoreTrailingSlash {
    path(routeHelper.projectsPathMatcher)(
      (get & extractUri)(listProjectsWith(ListProjectsRequest)) ~
      (post & extractUri)(createProject)
    ) ~
    (get & path(routeHelper.tutorialsPathMatcher) & extractUri)(
      listProjectsWith(ListTutorialsRequest)
    ) ~
    (get & path(routeHelper.thumbPathMatcher))(getThumb)
  }

  def serve: Future[Http.ServerBinding] = {
    Http().bindAndHandle(route, host, port)
  }
}

object Server {

  def main(args: Array[String]) {

    val config        = ConfigFactory.load.getConfig("project-manager")
    val serverConfig  = config.getConfig("server")
    val storageConfig = config.getConfig("storage")

    val host = serverConfig.getString("host")
    val port = serverConfig.getInt("port")

    val timeout =
      FiniteDuration(
        serverConfig.getDuration("timeout").toNanos,
        TimeUnit.NANOSECONDS
      )

    implicit val system: ActorSystem             = ActorSystem("project-manager")
    implicit val executor: ExecutionContext      = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val askTimeout: Timeout             = new Timeout(timeout)

    val localProjectsPath =
      new File(storageConfig.getString("local-projects-path"))
    val tmpProjectsPath = new File(
      storageConfig.getString("temporary-projects-path")
    )
    val tutorialsPath =
      new File(storageConfig.getString("tutorials-path"))
    val tutorialsCachePath =
      new File(storageConfig.getString("tutorials-cache-path"))

    val tutorialsDownloader =
      TutorialsDownloader(
        tutorialsPath,
        tutorialsCachePath,
        config.getString("tutorials.github-organisation")
      )
    val storageManager = StorageManager(
      localProjectsPath,
      tmpProjectsPath,
      tutorialsPath
    )

    val repoActor = system.spawn(
      ProjectsService.behavior(storageManager, tutorialsDownloader),
      "projects-repository"
    )

    val routeHelper = new RouteHelper
    val apiFactory  = ProjectFactory(routeHelper)

    val server = Server(host, port, repoActor, routeHelper, apiFactory)
    server.serve
  }
}
