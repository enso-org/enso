package org.enso.truffleloggerwrapper

import org.enso.logger.masking.Masking
import org.slf4j.ILoggerFactory

class TruffleLoggerWrapperFactory extends ILoggerFactory {
  override def getLogger(name: String) =
    new TruffleLoggerWrapper(name, Masking())
}
