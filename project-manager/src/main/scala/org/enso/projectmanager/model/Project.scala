package org.enso.projectmanager.model

import java.util.UUID

import org.enso.pkg.Package

import scala.collection.immutable.HashMap

sealed trait ProjectType {
  def isPersistent: Boolean
}
case object Local extends ProjectType {
  override def isPersistent: Boolean = true
}
case object Tutorial extends ProjectType {
  override def isPersistent: Boolean = false
}
case object Temporary extends ProjectType {
  override def isPersistent: Boolean = false
}

case class ProjectId(uid: UUID) {
  override def toString: String = uid.toString
}

case class Project(kind: ProjectType, pkg: Package) {
  def isPersistent: Boolean = kind.isPersistent
  def hasThumb:     Boolean = pkg.hasThumb
}

case class ProjectsRepository(projects: HashMap[ProjectId, Project]) {

  def getById(id: ProjectId): Option[Project] = {
    projects.get(id)
  }

  def insert(project: Project): (ProjectId, ProjectsRepository) = {
    val id      = ProjectsRepository.generateId
    val newRepo = copy(projects = projects + (id -> project))
    (id, newRepo)
  }
}

case object ProjectsRepository {

  def apply(projects: Seq[Project]): ProjectsRepository = {
    ProjectsRepository(HashMap(projects.map(generateId -> _): _*))
  }

  def generateId: ProjectId = ProjectId(UUID.randomUUID)
}
