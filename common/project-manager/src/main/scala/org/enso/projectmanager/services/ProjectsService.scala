package org.enso.projectmanager.services

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import org.enso.projectmanager.model.{Project, ProjectId, ProjectsRepository}

import scala.collection.immutable.HashMap

sealed trait ProjectsServiceCommand
sealed trait ProjectsCommand extends ProjectsServiceCommand
sealed trait ControlCommand  extends ProjectsServiceCommand

case class ListTutorialsRequest(replyTo: ActorRef[ListProjectsResponse])
    extends ProjectsCommand
case class ListProjectsRequest(replyTo: ActorRef[ListProjectsResponse])
    extends ProjectsCommand
case class ListProjectsResponse(projects: HashMap[ProjectId, Project])

case class GetProjectById(id: ProjectId, replyTo: ActorRef[GetProjectResponse])
    extends ProjectsCommand
case class GetProjectResponse(project: Option[Project])

case class CreateTemporary(
  name: String,
  replyTo: ActorRef[CreateTemporaryResponse])
    extends ProjectsCommand
case class CreateTemporaryResponse(id: ProjectId, project: Project)

case object TutorialsReady extends ProjectsServiceCommand

object ProjectsService {

  def behavior(
    storageManager: StorageManager,
    tutorialsDownloader: TutorialsDownloader
  ): Behavior[ProjectsServiceCommand] = Behaviors.setup { context =>
    Behaviors.withStash(capacity = 100) { buffer =>

      def handle(
        localRepo: ProjectsRepository,
        tutorialsRepo: Option[ProjectsRepository]
      ): Behavior[ProjectsServiceCommand] = Behaviors.receiveMessage {
        case ListProjectsRequest(replyTo) =>
          replyTo ! ListProjectsResponse(localRepo.projects)
          Behaviors.same
        case msg: ListTutorialsRequest =>
          tutorialsRepo match {
            case Some(repo) => msg.replyTo ! ListProjectsResponse(repo.projects)
            case None       => buffer.stash(msg)
          }
          Behaviors.same
        case GetProjectById(id, replyTo) =>
          val project =
            localRepo.getById(id).orElse(tutorialsRepo.flatMap(_.getById(id)))
          replyTo ! GetProjectResponse(project)
          Behaviors.same
        case TutorialsReady =>
          val newTutorialsRepo = storageManager.readTutorials
          buffer.unstashAll(handle(localRepo, Some(newTutorialsRepo)))
        case msg: CreateTemporary =>
          val project =
            storageManager.createTemporary(msg.name)
          val (projectId, newProjectsRepo) = localRepo.insert(project)
          msg.replyTo ! CreateTemporaryResponse(projectId, project)
          handle(newProjectsRepo, tutorialsRepo)
      }

      context.pipeToSelf(tutorialsDownloader.run())(_ => TutorialsReady)

      handle(storageManager.readLocalProjects, None)
    }
  }
}
