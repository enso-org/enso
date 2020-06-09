package org.enso.polyglot.debugger

import java.nio.ByteBuffer

import org.enso.polyglot.debugger.protocol.{
  ExceptionRepresentation,
  ObjectRepresentation
}
import org.graalvm.polyglot.io.MessageEndpoint

/**
  * Class that can be returned by serverTransport to establish communication
  * with the ReplDebuggerInstrument.
  */
class DebuggerSessionManagerEndpoint(
  val sessionManager: SessionManager,
  val peer: MessageEndpoint
) extends MessageEndpoint {
  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit =
    Debugger.deserializeResponse(data) match {
      case Right(response) =>
        handleResponse(response)
      case Left(error) =>
        throw error
    }

  override def sendPing(data: ByteBuffer): Unit = peer.sendPong(data)

  override def sendPong(data: ByteBuffer): Unit = {}

  override def sendClose(): Unit = {}

  private val executorStack
    : collection.mutable.Stack[ReplExecutorImplementation] =
    collection.mutable.Stack.empty

  private def currentExecutor: Option[ReplExecutorImplementation] =
    executorStack.headOption

  private def startNewSession(): Nothing = {
    val newExecutor = new ReplExecutorImplementation
    executorStack.push(newExecutor)
    sessionManager.startSession(newExecutor)
  }

  private def endMostNestedSession(
    requestingExecutor: ReplExecutorImplementation
  ): Unit = {
    if (!currentExecutor.contains(requestingExecutor)) {
      throw new IllegalStateException(
        "Session termination requested not from the most nested session"
      )
    } else {
      executorStack.pop()
    }
  }

  private def handleResponse(response: Response): Unit =
    if (response == SessionStartNotification) {
      startNewSession()
    } else {
      currentExecutor match {
        case Some(executor) =>
          executor.onResponse(response)
        case None =>
          throw new IllegalStateException(
            s"Unexpected response $response, but no session is running"
          )
      }
    }

  private class ReplExecutorImplementation extends ReplExecutor {
    var evaluationResult
      : Either[ExceptionRepresentation, ObjectRepresentation] = _
    override def evaluate(
      expression: String
    ): Either[ExceptionRepresentation, ObjectRepresentation] = {
      ensureUsable()
      evaluationResult = null
      peer.sendBinary(Debugger.createEvaluationRequest(expression))
      if (evaluationResult == null)
        throw new IllegalStateException(
          "DebuggerServer returned but did not send back expected result"
        )
      else
        evaluationResult
    }

    var bindingsResult: Map[String, ObjectRepresentation] = _
    override def listBindings(): Map[String, ObjectRepresentation] = {
      ensureUsable()
      bindingsResult = null
      peer.sendBinary(Debugger.createListBindingsRequest())
      if (bindingsResult == null)
        throw new IllegalStateException(
          "DebuggerServer returned but did not send back expected result"
        )
      else
        bindingsResult
    }

    var exited: Boolean = false
    def ensureUsable(): Unit = {
      if (exited) {
        throw new IllegalStateException(
          "Cannot use the executor after exit() has been called"
        )
      }
    }

    override def exit(): Nothing = {
      ensureUsable()
      // Note [Debugger Session Exit Return]
      endMostNestedSession(this)
      exited = true
      peer.sendBinary(Debugger.createSessionExitRequest())
      throw new IllegalStateException(
        "DebuggerServer unexpectedly returned from exit"
      )
    }

    /* Note [Debugger Session Exit Return]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * Sending exit request throws an exception that returns control to the
     * interpreter. Sending the request using the synchronous sendBinary
     * function means that this function will never return. So cleanup has to be
     * done before calling it.
     */

    def onResponse(response: Response): Unit = {
      response match {
        case EvaluationSuccess(result)    => evaluationResult = Right(result)
        case EvaluationFailure(exception) => evaluationResult = Left(exception)
        case ListBindingsResult(bindings) => bindingsResult   = bindings
        case SessionStartNotification =>
          throw new IllegalStateException(
            "Session start notification sent while the session is already" +
            " running"
          )
      }
    }
  }
}
