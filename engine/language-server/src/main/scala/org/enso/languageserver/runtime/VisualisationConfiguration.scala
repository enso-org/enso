package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.logger.masking.{MaskedString, ToLogString}

/** A configuration object for properties of the visualisation.
  *
  * @param executionContextId an execution context of the visualisation
  * @param visualisationModule a qualified name of the module containing
  *                            the expression which creates visualisation
  * @param expression the expression that creates a visualisation
  */
case class VisualisationConfiguration(
  executionContextId: UUID,
  visualisationModule: String,
  expression: String
) extends ToLogString {

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String =
    "VisualisationConfiguration(" +
    s"executionContextId=$executionContextId," +
    s"visualisationModule=$visualisationModule,expression=" +
    MaskedString(expression).toLogString(shouldMask) +
    ")"
}
