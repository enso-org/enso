package org.enso.languageserver.boot

import org.enso.distribution.{DistributionManager, Environment}
import org.enso.logging.service.LoggingSetupHelper
import org.slf4j.event.Level

import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global

/** A helper for setting up the logging service. */
object Logging extends LoggingSetupHelper(global) {

  /** The default [[Environment]] implementation, with no overrides. */
  private lazy val environment: Environment = new Environment {}

  /** The [[DistributionManager]] instance. */
  private lazy val distributionManager = new DistributionManager(environment)

  /** @inheritdoc */
  override val defaultLogLevel: Level = Level.INFO

  /** @inheritdoc */
  override lazy val logPath: Path = distributionManager.paths.logs

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-language-server"
}
