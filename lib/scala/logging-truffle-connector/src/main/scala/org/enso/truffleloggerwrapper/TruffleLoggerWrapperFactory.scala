package org.enso.truffleloggerwrapper

import org.enso.logger.masking.Masking
import org.slf4j.ILoggerFactory

/** A factory for [[TruffleLoggerWrapper]]. */
class TruffleLoggerWrapperFactory extends ILoggerFactory {

  /** @inheritdoc */
  override def getLogger(name: String) =
    new TruffleLoggerWrapper(name, Masking())
}
