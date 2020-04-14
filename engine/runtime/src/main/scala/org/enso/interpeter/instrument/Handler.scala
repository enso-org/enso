package org.enso.interpeter.instrument

import java.nio.ByteBuffer
import java.util.UUID
import java.util.function.Consumer

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.instrument.IdExecutionInstrument.{
  ExpressionCall,
  ExpressionValue
}
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.interpreter.service.ExecutionService
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.io.MessageEndpoint

/**
  * A message endpoint implementation used by the
  * [[org.enso.interpreter.instrument.RuntimeServerInstrument]].
  * @param handler
  */
class Endpoint(handler: Handler) extends MessageEndpoint {
  var client: MessageEndpoint = _

  /**
    * Sets the client end of the connection, after it has been established.
    *
    * @param ep the client endpoint.
    */
  def setClient(ep: MessageEndpoint): Unit = client = ep

  /**
    * Sends a response to the connected client.
    *
    * @param msg the message to send.
    */
  def sendToClient(msg: Api.Response): Unit =
    client.sendBinary(Api.serialize(msg))

  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit =
    Api.deserializeRequest(data).foreach(handler.onMessage)

  override def sendPing(data: ByteBuffer): Unit = client.sendPong(data)

  override def sendPong(data: ByteBuffer): Unit = {}

  override def sendClose(): Unit = {}
}

/**
  * A message handler, dispatching behaviors based on messages received
  * from an instance of [[Endpoint]].
  */
class Handler {
  val endpoint       = new Endpoint(this)
  val contextManager = new ExecutionContextManager

  var executionService: ExecutionService = _
  var truffleContext: TruffleContext     = _

  /**
    * Initializes the handler with relevant Truffle objects, allowing it to
    * perform code execution.
    *
    * @param service the language execution service instance.
    * @param context the current Truffle context.
    */
  def initializeExecutionService(
    service: ExecutionService,
    context: TruffleContext
  ): Unit = {
    executionService = service
    truffleContext   = context
    endpoint.sendToClient(Api.Response(Api.InitializedNotification()))
  }

  sealed private trait ExecutionItem

  private object ExecutionItem {
    case class Method(
      module: String,
      constructor: String,
      function: String
    ) extends ExecutionItem

    case class CallData(callData: FunctionCall) extends ExecutionItem
  }
  private def sendVal(res: ExpressionValue): Unit = {
    endpoint.sendToClient(
      Api.Response(
        Api.ExpressionValueUpdateNotification(
          res.getExpressionId,
          res.getValue.toString
        )
      )
    )
  }

  private def execute(
    executionItem: ExecutionItem,
    furtherStack: List[UUID]
  ): Unit = {
    var enterables: Map[UUID, FunctionCall] = Map()
    val valsCallback: Consumer[ExpressionValue] =
      if (furtherStack.isEmpty) sendVal else _ => ()
    val callablesCallback: Consumer[ExpressionCall] = fun =>
      enterables += fun.getExpressionId -> fun.getCall
    executionItem match {
      case ExecutionItem.Method(module, cons, function) =>
        executionService.execute(
          module,
          cons,
          function,
          valsCallback,
          callablesCallback
        )
      case ExecutionItem.CallData(callData) =>
        executionService.execute(callData, valsCallback, callablesCallback)
    }

    furtherStack match {
      case Nil => ()
      case item :: tail =>
        enterables
          .get(item)
          .foreach(call => execute(ExecutionItem.CallData(call), tail))
    }
  }

  private def withContext(action: => Unit): Unit = {
    val token = truffleContext.enter()
    try {
      action
    } finally {
      truffleContext.leave(token)
    }
  }

  /**
    * Handles a message received from the client.
    *
    * @param msg the message to handle.
    */
  def onMessage(msg: Api.Request): Unit = msg match {
    case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
      contextManager.create(contextId)
      endpoint.sendToClient(
        Api.Response(requestId, Api.CreateContextResponse(contextId))
      )

    case Api.Request(requestId, Api.DestroyContextRequest(contextId)) =>
      if (contextManager.get(contextId).isDefined) {
        contextManager.destroy(contextId)
        endpoint.sendToClient(
          Api.Response(requestId, Api.DestroyContextResponse(contextId))
        )
      } else {
        endpoint.sendToClient(
          Api.Response(requestId, Api.ContextNotExistError(contextId))
        )
      }

    case Api.Request(requestId, Api.PushContextRequest(contextId, item)) => {
      val payload = contextManager.push(contextId, item) match {
        case Some(()) => Api.PushContextResponse(contextId)
        case None     => Api.ContextNotExistError(contextId)
      }
      endpoint.sendToClient(Api.Response(requestId, payload))
    }

    case Api.Request(requestId, Api.PopContextRequest(contextId)) =>
      if (contextManager.get(contextId).isDefined) {
        val payload = contextManager.pop(contextId) match {
          case Some(_) => Api.PopContextResponse(contextId)
          case None    => Api.EmptyStackError(contextId)
        }
        endpoint.sendToClient(Api.Response(requestId, payload))
      } else {
        endpoint.sendToClient(
          Api.Response(requestId, Api.ContextNotExistError(contextId))
        )
      }

    case Api.Request(_, Api.Execute(mod, cons, fun, furtherStack)) =>
      withContext(execute(ExecutionItem.Method(mod, cons, fun), furtherStack))

  }
}
