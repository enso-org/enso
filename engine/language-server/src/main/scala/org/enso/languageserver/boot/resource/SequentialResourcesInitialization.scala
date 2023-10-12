package org.enso.languageserver.boot.resource
import scala.concurrent.{ExecutionContext, Future}

/** Initializes resources in sequence.
  *
  * @param resources the list of resources to initialize
  */
final class SequentialResourcesInitialization(
  resources: Seq[InitializationComponent]
)(implicit ec: ExecutionContext)
    extends InitializationComponent {

  /** @inheritdoc */
  override def isInitialized: Boolean =
    resources.forall(_.isInitialized)

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    resources.foldLeft(Future.successful(InitializationComponent.Initialized)) {
      (action, resource) =>
        action.flatMap { _ =>
          if (resource.isInitialized)
            Future.successful(InitializationComponent.Initialized)
          else resource.init()
        }
    }
}

object SequentialResourcesInitialization {

  /** Create a sequential initialization component from a sequence of resources.
    *
    * @param resources the list of resources to initialize.
    * @param ec the execution context
    * @return the sequential initialization component
    */
  def apply(resources: InitializationComponent*)(implicit
    ec: ExecutionContext
  ): SequentialResourcesInitialization =
    new SequentialResourcesInitialization(resources)
}
