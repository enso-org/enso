package org.enso.projectmanager.infrastructure.http

import akka.http.scaladsl.model.{ContentType, HttpEntity, MediaTypes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import org.enso.jsonrpc.Endpoint

final class ProjectsEndpoint extends Endpoint {

  /** @inheritdoc */
  override def route: Route =
    projectsEndpoint

  private val projectsEndpoint = {
    path("projects" / JavaUUID / "enso-project") { projectId =>
      get {
        complete(
          HttpEntity(
            ContentType(MediaTypes.`application/zip`),
            projectId.toString.getBytes
          )
        )
      }
    }
  }

}
