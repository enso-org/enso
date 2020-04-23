package org.enso.languageserver.runtime

import java.util.UUID

object VisualisationProtocol {

  /**
    * Represents a visualisation context.
    *
    * @param visualisationId a visualisation identifier
    * @param contextId a context identifier
    * @param expressionId an expression identifier
    */
  case class VisualisationContext(
    visualisationId: UUID,
    contextId: UUID,
    expressionId: UUID
  )

  /**
    * An event signaling a visualisation update.
    *
    * @param visualisationContext a visualisation context
    * @param data a visualisation data
    */
  case class VisualisationUpdate(
    visualisationContext: VisualisationContext,
    data: Array[Byte]
  )

}
