package org.enso.projectmanager.infrastructure.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
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
          HttpEntity(ContentTypes.`text/plain(UTF-8)`, projectId.toString)
        )
      }
    }
  }

}
