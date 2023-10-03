package org.enso.projectmanager.service

import java.net.URI
import scala.concurrent.Future

/** A service descriptor that provides information on the logging service setup.
  */
trait LoggingServiceDescriptor {

  /** Returns a future that will yield the logging service endpoint once it is
    * initialized or None if the logging service does not expect incoming
    * connections.
    */
  def getEndpoint: Future[Option[URI]]
}
