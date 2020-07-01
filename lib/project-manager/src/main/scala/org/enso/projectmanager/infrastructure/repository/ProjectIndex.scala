package org.enso.projectmanager.infrastructure.repository

import java.util.UUID

import org.enso.projectmanager.data.Default
import org.enso.projectmanager.model.Project

/**
  * A helper data object enabling indexing of projects.
  *
  * @param projects user projects index
  */
case class ProjectIndex(projects: Map[UUID, Project] = Map.empty) {

  /**
    * Upserts a project to the index.
    *
    * @param project the project to add
    * @return an updated project
    */
  def upsert(project: Project): ProjectIndex =
    ProjectIndex(projects + (project.id -> project))

  /**
    * Updates a project inside the index using a modifcation function.
    *
    * @param id the project id
    * @param f the modification functionProjectRenameHandler
    */
  def update(id: UUID)(f: Project => Project): ProjectIndex =
    ProjectIndex(projects + (id -> f(projects(id))))

  /**
    * Removes a project.
    *
    * @param projectId the project id to remove
    * @return an updated project
    */
  def remove(projectId: UUID): ProjectIndex =
    ProjectIndex(projects - projectId)

  /**
    * Finds user project by ID.
    *
    * @param projectId a project id
    * @return optional project
    */
  def findById(projectId: UUID): Option[Project] =
    projects.get(projectId)

  /**
    * Queries index using a function that specifies criteria of result set.
    *
    * @param predicate a predicate function
    * @return projects that meet the criteria
    */
  def find(predicate: Project => Boolean): List[Project] =
    projects.values.filter(predicate).toList

  /**
    * Checks if project with the provided name is in the index.
    *
    * @param name a project name
    * @return true if exists
    */
  def exists(name: String): Boolean = projects.values.exists(_.name == name)

}

object ProjectIndex {

  val Empty = ProjectIndex()

  implicit val indexDefault: Default[ProjectIndex] = Default.Val(Empty)

}
