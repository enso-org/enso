package org.enso.languageserver.boot.resource
import org.enso.polyglot.LanguageInfo
import org.graalvm.polyglot.Context
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

/** Initialize the Truffle context.
  *
  * @param truffleContext the Truffle context
  */
class TruffleContextInitialization(truffleContext: Context)(implicit
  ec: ExecutionContext
) extends InitializationComponent {

  private val log = LoggerFactory.getLogger(this.getClass)

  /** @inheritdoc */
  override def init(): Future[InitializationComponent.Initialized.type] =
    Future {
      truffleContext.initialize(LanguageInfo.ID)
      log.info("Initialized Runtime context.")
      InitializationComponent.Initialized
    }
}
