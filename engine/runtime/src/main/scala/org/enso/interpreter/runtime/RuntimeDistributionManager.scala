package org.enso.interpreter.runtime

import org.enso.distribution.{DistributionManager, Environment}

/** A [[DistributionManager]] for the runtime.
  *
  * It is used to resolve paths to library cache etc.
  */
object RuntimeDistributionManager
    extends DistributionManager(new Environment {})
