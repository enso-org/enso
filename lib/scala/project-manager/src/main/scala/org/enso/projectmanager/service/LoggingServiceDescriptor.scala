package org.enso.projectmanager.service

import akka.http.scaladsl.model.Uri

import scala.concurrent.Future

/** A service descriptor that provides information on the logging service setup.
  */
trait LoggingServiceDescriptor {

  /** Returns a future that will yield the logging service endpoint once it is
    * initialized or None if the logging service does not expect incoming
    * connections.
    */
  def getEndpoint: Future[Option[Uri]]
}
