package org.enso.languageserver.boot.resource

import scala.concurrent.Future

/** A component that should be initialized. */
trait InitializationComponent {

  /** Initialize the component. */
  def init(): Future[InitializationComponent.Initialized.type]
}

object InitializationComponent {

  case object Initialized
}
