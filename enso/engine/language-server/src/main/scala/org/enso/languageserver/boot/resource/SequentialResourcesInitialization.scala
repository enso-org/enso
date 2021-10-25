package org.enso.languageserver.boot.resource
import scala.concurrent.{ExecutionContext, Future}

/** Initializes resources in sequence.
  *
  * @param resources the list of resources to initialize
  */
class SequentialResourcesInitialization(
  resources: Seq[InitializationComponent]
)(implicit ec: ExecutionContext)
    extends InitializationComponent {

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    resources.foldLeft(Future.successful(InitializationComponent.Initialized)) {
      (action, resource) => action.flatMap(_ => resource.init())
    }
}

object SequentialResourcesInitialization {

  def apply(resources: InitializationComponent*)(implicit
    ec: ExecutionContext
  ): SequentialResourcesInitialization =
    new SequentialResourcesInitialization(resources)
}
