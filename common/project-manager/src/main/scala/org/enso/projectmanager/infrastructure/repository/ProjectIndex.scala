package org.enso.projectmanager.infrastructure.repository

import java.util.UUID

import org.enso.projectmanager.data.Default
import org.enso.projectmanager.model.Project

/**
  * A helper data object enabling indexing of projects.
  *
  * @param userProjects user projects index
  * @param sampleProjects sample project index
  * @param temporaryProjects temporary projects index
  */
case class ProjectIndex(
  userProjects: Map[UUID, Project]      = Map.empty,
  sampleProjects: List[Project]         = List.empty,
  temporaryProjects: Map[UUID, Project] = Map.empty
) {

  /**
    * Adds user project to the index.
    *
    * @param project the project to add
    * @return an updated project
    */
  def addUserProject(project: Project): ProjectIndex =
    ProjectIndex(userProjects + (project.id -> project))

  /**
    * Removes a user project.
    *
    * @param projectId the project id to remove
    * @return an updated project
    */
  def removeUserProject(projectId: UUID): ProjectIndex =
    ProjectIndex(userProjects - projectId)

  /**
    * Finds user project by ID.
    *
    * @param projectId a project id
    * @return optional project
    */
  def findUserProject(projectId: UUID): Option[Project] =
    userProjects.get(projectId)

  /**
    * Checks if project with the provided name is in the index.
    *
    * @param name a project name
    * @return true if exists
    */
  def exists(name: String): Boolean = userProjects.values.exists(_.name == name)

}

object ProjectIndex {

  val Empty = ProjectIndex()

  implicit val indexDefault: Default[ProjectIndex] = Default.Val(Empty)

}
