package org.enso.languageserver.boot.resource

import scala.concurrent.{ExecutionContext, Future}

/** Initializes resources in parallel.
  *
  * @param resources the list of resources to initialize
  * @param ec the execution context
  */
class AsyncResourcesInitialization(
  resources: Iterable[InitializationComponent]
)(implicit ec: ExecutionContext)
    extends InitializationComponent {

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future
      .traverse(resources)(_.init())
      .map { _ => InitializationComponent.Initialized }
}

object AsyncResourcesInitialization {

  /** Create [[AsyncResourcesInitialization]] component.
    *
    * @param resources the list of resources to initialize
    * @param ec the execution context
    * @return new async initialization component
    */
  def apply(resources: InitializationComponent*)(implicit
    ec: ExecutionContext
  ): AsyncResourcesInitialization =
    new AsyncResourcesInitialization(resources)
}
