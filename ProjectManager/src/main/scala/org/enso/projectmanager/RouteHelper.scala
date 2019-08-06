package org.enso.projectmanager

import java.util.UUID

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher0
import akka.http.scaladsl.server.PathMatcher1
import akka.http.scaladsl.server.PathMatchers.JavaUUID
import org.enso.projectmanager.model.ProjectId

class RouteHelper {

  val tutorials: String = "tutorials"
  val projects: String  = "projects"
  val thumb: String     = "thumb"

  val tutorialsPath: Path                = Path / tutorials
  val tutorialsPathMatcher: PathMatcher0 = tutorials

  val projectsPath: Path                = Path / projects
  val projectsPathMatcher: PathMatcher0 = projects

  def projectPath(id: ProjectId): Path = projectsPath / id.toString

  val projectPathMatcher: PathMatcher1[ProjectId] =
    (projectsPathMatcher / JavaUUID).map(ProjectId)

  def thumbPath(id: ProjectId): Path = projectPath(id) / thumb
  val thumbPathMatcher: PathMatcher1[ProjectId] = projectPathMatcher / thumb

  def uriFor(base: Uri, path: Path): Uri = base.withPath(path)
}
