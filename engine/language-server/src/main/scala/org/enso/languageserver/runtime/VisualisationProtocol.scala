package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.languageserver.data.ClientId

object VisualisationProtocol {

  /**
    * Requests the language server to attach a visualisation to the expression
    * specified by `expressionId`.
    *
    * @param clientId the requester id
    * @param visualisationId an identifier of a visualisation
    * @param expressionId an identifier of an expression which is visualised
    * @param visualisationConfig a configuration object for properties of the
    *                            visualisation
    */
  case class AttachVisualisation(
    clientId: ClientId,
    visualisationId: UUID,
    expressionId: UUID,
    visualisationConfig: VisualisationConfiguration
  )

  /**
    * Signals that attaching a visualisation has succeeded.
    */
  case object VisualisationAttached

  /**
    * Requests the language server to detach a visualisation from the expression
    * specified by `expressionId`.
    *
    * @param clientId the requester id
    * @param contextId an execution context identifier
    * @param visualisationId an identifier of a visualisation
    * @param expressionId an identifier of an expression which is visualised
    */
  case class DetachVisualisation(
    clientId: ClientId,
    contextId: UUID,
    visualisationId: UUID,
    expressionId: UUID
  )

  /**
    * Signals that detaching a visualisation has succeeded.
    */
  case object VisualisationDetached

  /**
    * Requests the language server to modify a visualisation.
    *
    * @param clientId  the requester id
    * @param visualisationId     an identifier of a visualisation
    * @param visualisationConfig a configuration object for properties of the
    *                            visualisation
    */
  case class ModifyVisualisation(
    clientId: ClientId,
    visualisationId: UUID,
    visualisationConfig: VisualisationConfiguration
  )

  /**
    * Signals that a visualisation modification has succeeded.
    */
  case object VisualisationModified

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
