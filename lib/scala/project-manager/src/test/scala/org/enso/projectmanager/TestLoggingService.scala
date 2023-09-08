package org.enso.projectmanager

import org.enso.projectmanager.service.LoggingServiceDescriptor
import java.net.URI

import scala.concurrent.Future

class TestLoggingService extends LoggingServiceDescriptor {
  private var currentFuture: Future[Option[URI]] = Future.successful(None)

  override def getEndpoint: Future[Option[URI]] = currentFuture

  def withOverriddenEndpoint[R](
    future: Future[Option[URI]]
  )(action: => R): Unit = {
    val oldValue = currentFuture
    currentFuture = future
    try {
      action
    } finally {
      currentFuture = oldValue
    }
  }
}
