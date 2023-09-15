package org.enso.languageserver.protocol.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  VisualizationContext => BinaryVisualizationContext,
  VisualizationUpdate => BinaryVisualizationUpdate
}
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  VisualizationContext,
  VisualizationUpdate
}

object VisualizationUpdateFactory {

  /** Creates a [[VisualizationUpdate]] inside a [[FlatBufferBuilder]].
    *
    * @param update a visualization update
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create(update: VisualizationUpdate)(implicit
    builder: FlatBufferBuilder
  ): Int = {
    val ctx = createVisualizationCtx(update.visualizationContext)
    val data =
      BinaryVisualizationUpdate.createDataVector(builder, update.data)
    BinaryVisualizationUpdate.createVisualizationUpdate(
      builder,
      ctx,
      data
    )
  }

  /** Creates a [[VisualizationContext]] inside a [[FlatBufferBuilder]].
    *
    * @param ctx a VisualizationContext
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createVisualizationCtx(ctx: VisualizationContext)(implicit
    builder: FlatBufferBuilder
  ): Int = {
    BinaryVisualizationContext.startVisualizationContext(builder)
    BinaryVisualizationContext.addContextId(
      builder,
      EnsoUuidFactory.create(ctx.contextId)
    )
    BinaryVisualizationContext.addExpressionId(
      builder,
      EnsoUuidFactory.create(ctx.expressionId)
    )
    BinaryVisualizationContext.addVisualizationId(
      builder,
      EnsoUuidFactory.create(ctx.visualizationId)
    )
    BinaryVisualizationContext.endVisualizationContext(builder)
  }

}
