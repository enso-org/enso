package org.enso.languageserver.protocol.data.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.executioncontext
import org.enso.languageserver.runtime.VisualisationProtocol.{
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
      executioncontext.VisualisationUpdate
        .createDataVector(builder, update.data)
    executioncontext.VisualisationUpdate.createVisualisationUpdate(
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
    executioncontext.VisualisationContext.startVisualisationContext(builder)
    executioncontext.VisualisationContext
      .addContextId(builder, EnsoUuidFactory.create(ctx.contextId))
    executioncontext.VisualisationContext
      .addExpressionId(builder, EnsoUuidFactory.create(ctx.expressionId))
    executioncontext.VisualisationContext
      .addVisualisationId(builder, EnsoUuidFactory.create(ctx.visualisationId))
    executioncontext.VisualisationContext.endVisualisationContext(builder)
  }

}
