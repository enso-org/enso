package org.enso.projectmanager

import akka.http.scaladsl.model.Uri
import org.enso.projectmanager.service.LoggingServiceDescriptor

import scala.concurrent.Future

class TestLoggingService extends LoggingServiceDescriptor {
  private var currentFuture: Future[Option[Uri]] = Future.successful(None)

  override def getEndpoint: Future[Option[Uri]] = currentFuture

  def withOverriddenEndpoint[R](
    future: Future[Option[Uri]]
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
