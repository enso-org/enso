package org.enso.projectmanager.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.Uri
import org.enso.projectmanager.{RouteHelper, model}
import org.enso.projectmanager.model.ProjectId
import spray.json.DefaultJsonProtocol

case class Project(
  id: String,
  name: String,
  path: String,
  thumb: Option[String],
  persisted: Boolean)

case class ProjectFactory(routeHelper: RouteHelper) {

  def fromModel(
    id: ProjectId,
    project: model.Project,
    baseUri: Uri
  ): Project = {
    val thumbUri =
      if (project.hasThumb)
        Some(routeHelper.uriFor(baseUri, routeHelper.thumbPath(id)))
      else None
    Project(
      id.toString,
      project.pkg.name,
      project.pkg.root.getAbsolutePath,
      thumbUri.map(_.toString),
      project.isPersistent
    )
  }
}

trait ProjectJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val projectFormat = jsonFormat5(Project.apply)
}
