package org.enso.interpreter.instrument

import java.nio.ByteBuffer
import org.enso.interpreter.instrument.ReplDebuggerInstrument.ReplExecutionEventNode
import org.enso.polyglot.debugger.{
  Debugger,
  EvaluationRequest,
  ListBindingsRequest,
  Request,
  SessionExitRequest
}
import org.graalvm.polyglot.io.MessageEndpoint

/**
  * Helper class that handles communication with Debugger client and delegates
  * request to the execution event node of the ReplDebuggerInstrument.
  */
class DebuggerMessageHandler extends MessageEndpoint {
  private var client: MessageEndpoint = _

  /**
    * Sets the client end of the connection, after it has been established.
    *
    * @param ep the client endpoint.
    */
  def setClient(ep: MessageEndpoint): Unit = client = ep

  /**
    * Checks if a client has been registered.
    *
    * @return a boolean value indicating whether a client is registered
    */
  def hasClient: Boolean = client != null

  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit = {
    Debugger.deserializeRequest(data) match {
      case Right(request) => onMessage(request)
      case Left(error)    => throw error
    }
  }

  override def sendPing(data: ByteBuffer): Unit = client.sendPong(data)

  override def sendPong(data: ByteBuffer): Unit = {}

  override def sendClose(): Unit = {}

  private def sendToClient(data: ByteBuffer): Unit = {
    client.sendBinary(data)
  }

  private val executionNodeStack
    : collection.mutable.Stack[ReplExecutionEventNode] =
    collection.mutable.Stack.empty

  private def currentExecutionNode: Option[ReplExecutionEventNode] =
    executionNodeStack.headOption

  /**
    * Starts a REPL session by sending a message to the client.
    *
    * @param executionNode execution node used for instrumenting the session
    */
  def startSession(executionNode: ReplExecutionEventNode): Unit = {
    executionNodeStack.push(executionNode)
    sendToClient(Debugger.createSessionStartNotification())
  }

  /**
    * A helper function that cleans up the current session and terminates it.
    *
    * @return never returns as control is passed to the interpreter
    */
  private def endSession(): Nothing = {
    val node = executionNodeStack.pop()
    node.exit()
    throw new IllegalStateException(
      "exit() on execution node returned unexpectedly"
    )
  }

  private def onMessage(request: Request): Unit =
    currentExecutionNode match {
      case Some(node) =>
        request match {
          case EvaluationRequest(expression) =>
            val result = node.evaluate(expression)
            result match {
              case Left(error) =>
                sendToClient(Debugger.createEvaluationFailure(error))
              case Right(value) =>
                sendToClient(Debugger.createEvaluationSuccess(value))
            }
          case ListBindingsRequest =>
            val bindings = node.listBindings()
            sendToClient(Debugger.createListBindingsResult(bindings))
          case SessionExitRequest =>
            endSession()
        }
      case None =>
        throw new IllegalStateException(
          "Got a request but no session is running"
        )
    }
}
