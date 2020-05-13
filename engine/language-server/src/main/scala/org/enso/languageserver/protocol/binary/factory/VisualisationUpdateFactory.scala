package org.enso.languageserver.protocol.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  VisualisationContext => BinaryVisualisationContext,
  VisualisationUpdate => BinaryVisualisationUpdate
}
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  VisualisationContext,
  VisualisationUpdate
}

object VisualisationUpdateFactory {

  /**
    * Creates a [[VisualisationUpdate]] inside a [[FlatBufferBuilder]].
    *
    * @param update a visualisation update
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create(update: VisualisationUpdate)(
    implicit builder: FlatBufferBuilder
  ): Int = {
    val ctx = createVisualisationCtx(update.visualisationContext)
    val data =
      BinaryVisualisationUpdate.createDataVector(builder, update.data)
    BinaryVisualisationUpdate.createVisualisationUpdate(
      builder,
      ctx,
      data
    )
  }

  /**
    * Creates a [[VisualisationContext]] inside a [[FlatBufferBuilder]].
    *
    * @param ctx a VisualisationContext
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createVisualisationCtx(ctx: VisualisationContext)(
    implicit builder: FlatBufferBuilder
  ): Int = {
    BinaryVisualisationContext.startVisualisationContext(builder)
    BinaryVisualisationContext.addContextId(
      builder,
      EnsoUuidFactory.create(ctx.contextId)
    )
    BinaryVisualisationContext.addExpressionId(
      builder,
      EnsoUuidFactory.create(ctx.expressionId)
    )
    BinaryVisualisationContext.addVisualisationId(
      builder,
      EnsoUuidFactory.create(ctx.visualisationId)
    )
    BinaryVisualisationContext.endVisualisationContext(builder)
  }

}
