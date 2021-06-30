package org.enso.interpreter.runtime

import org.enso.distribution.{DistributionManager, Environment}

object RuntimeDistributionManager
    extends DistributionManager(new Environment {})
