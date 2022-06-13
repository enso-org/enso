package org.enso.interpreter.instrument

import java.nio.ByteBuffer
import com.oracle.truffle.api.TruffleStackTrace
import com.typesafe.scalalogging.Logger
import org.enso.polyglot.debugger._
import org.graalvm.polyglot.io.MessageEndpoint

import scala.jdk.CollectionConverters._

/** Helper class that handles communication with Debugger client and delegates
  * request to the execution event node of the ReplDebuggerInstrument.
  */
class DebuggerMessageHandler extends MessageEndpoint {
  private var client: MessageEndpoint = _

  /** Sets the client end of the connection, after it has been established.
    *
    * @param ep the client endpoint.
    */
  def setClient(ep: MessageEndpoint): Unit = client = ep

  /** Checks if a client has been registered.
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

  /** Starts a REPL session by sending a message to the client.
    *
    * @param executionNode execution node used for instrumenting the session
    */
  def startSession(executionNode: ReplExecutionEventNode): Unit = {
    executionNodeStack.push(executionNode)
    sendToClient(Debugger.createSessionStartNotification())
  }

  /** A helper function that cleans up the current session and terminates it.
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
              case Left(exception) =>
                val exceptionWithTraces =
                  DebuggerMessageHandler
                    .fillInTruffleStackTraceForSerialization(
                      exception
                    )
                sendToClient(
                  Debugger.createEvaluationFailure(exceptionWithTraces)
                )
              case Right(value) =>
                node.showObject(value) match {
                  case Left(error) =>
                    Logger[this.type].warn(
                      s"Failed to call `to_text` on computation result (${error.getMessage}), falling back to Java representation."
                    )
                    sendToClient(Debugger.createEvaluationSuccess(value))
                  case Right(repr: String) =>
                    sendToClient(Debugger.createEvaluationSuccess(repr))
                }
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

object DebuggerMessageHandler {

  /** Converts the attached Truffle stack trace into StackTraceElements that are
    * attached to the exception, so that they are preserved by serialization.
    *
    * The language stack trace is attached to the last exception in the
    * exception chain if that exception has empty stack trace. In normal
    * conditions, if there are Truffle frames attached, the last exception in
    * the chain will be TruffleStackTrace.LazyStackTrace which has empty Java
    * stack trace.
    *
    * @param exception the exception that was thrown during evaluation
    * @return returns the original exception which has been possibly modified
    */
  private def fillInTruffleStackTraceForSerialization(
    exception: Exception
  ): Exception = {
    val truffleTrace =
      Option(TruffleStackTrace.getStackTrace(exception))
        .map(_.asScala)
        .getOrElse(Nil)

    val javaTrace = truffleTrace.map { elem =>
      val rootNode = elem.getTarget.getRootNode
      val language =
        Option(rootNode.getLanguageInfo).map(_.getId).getOrElse("unknown")
      val declaringClass = s"<$language>"
      val methodName = Option(rootNode.getQualifiedName)
        .getOrElse("?")
      val sourceLocation = for {
        sourceSection <- Option(rootNode.getSourceSection)
        source        <- Option(sourceSection.getSource)
      } yield (source.getName, sourceSection.getStartLine)
      val (fileName, lineNumber) = sourceLocation.getOrElse((null, -1))
      new StackTraceElement(declaringClass, methodName, fileName, lineNumber)
    }

    if (javaTrace.nonEmpty) {
      var lastException: Throwable = exception
      while (lastException.getCause != null) {
        lastException = lastException.getCause
      }
      if (lastException.getStackTrace.isEmpty) {
        lastException.setStackTrace(javaTrace.toArray)
      }
    }

    exception
  }
}
