package org.enso.polyglot.debugger

import java.nio.ByteBuffer
import scala.jdk.CollectionConverters._

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.polyglot.debugger.protocol.factory.{
  RequestFactory,
  ResponseFactory
}
import org.enso.polyglot.debugger.protocol.{
  Request => BinaryRequest,
  Response => BinaryResponse,
  RequestPayload,
  ResponsePayload
}

object Debugger {

  /**
    * Deserializes a byte buffer into a Request message.
    *
    * @param bytes the buffer to deserialize
    * @return the deserialized message, if the byte buffer can be deserialized.
    */
  def deserializeRequest(bytes: ByteBuffer): Option[Request] =
    try {
      val inMsg = BinaryRequest.getRootAsRequest(bytes)

      inMsg.payloadType() match {
        case RequestPayload.EVALUATE =>
          val evaluationRequest = inMsg
            .payload(new protocol.EvaluationRequest())
            .asInstanceOf[protocol.EvaluationRequest]
          Some(EvaluationRequest(evaluationRequest.expression()))
        case RequestPayload.LIST_BINDINGS =>
          Some(ListBindingsRequest)
        case RequestPayload.SESSION_EXIT =>
          Some(SessionExitRequest)
        case _ => None
      }
    } catch {
      case _: Exception => None
    }

  /**
    * Deserializes a byte buffer into a Response message.
    *
    * @param bytes the buffer to deserialize
    * @return the deserialized message, if the byte buffer can be deserialized.
    */
  def deserializeResponse(bytes: ByteBuffer): Option[Response] =
    try {
      val inMsg = BinaryResponse.getRootAsResponse(bytes)

      inMsg.payloadType() match {
        case ResponsePayload.EVALUATION_SUCCESS =>
          val evaluationResult = inMsg
            .payload(new protocol.EvaluationSuccess())
            .asInstanceOf[protocol.EvaluationSuccess]
          Some(EvaluationSuccess(evaluationResult.result()))
        case ResponsePayload.EVALUATION_FAILURE =>
          val evaluationResult = inMsg
            .payload(new protocol.EvaluationFailure())
            .asInstanceOf[protocol.EvaluationFailure]
          Some(EvaluationFailure(evaluationResult.exception()))
        case ResponsePayload.LIST_BINDINGS =>
          val bindingsResult = inMsg
            .payload(new protocol.ListBindingsResult())
            .asInstanceOf[protocol.ListBindingsResult]
          val bindings =
            for (i <- 0 until bindingsResult.bindingsLength()) yield {
              val binding = bindingsResult.bindings(i)
              (binding.name(), binding.value())
            }
          Some(ListBindingsResult(bindings.toMap))
        case ResponsePayload.SESSION_START =>
          Some(SessionStartNotification)
        case ResponsePayload.SESSION_EXIT =>
          Some(SessionExitSuccess)
        case _ => None
      }
    } catch {
      case _: Exception => None
    }

  /**
    * Creates an EvaluationRequest message in the form of a ByteBuffer that can
    * be sent to the debugger.
    *
    * @param expression expression to evaluate
    * @return the serialized message
    */
  def createEvaluationRequest(expression: String): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(256)
    val requestOffset                       = RequestFactory.createEvaluationRequest(expression)
    val outMsg = BinaryRequest.createRequest(
      builder,
      RequestPayload.EVALUATE,
      requestOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates a ListBindingsRequest message in the form of a ByteBuffer that can
    * be sent to the debugger.
    *
    * @return the serialized message
    */
  def createListBindingsRequest(): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(64)
    val requestOffset                       = RequestFactory.createListBindingsRequest()
    val outMsg = BinaryRequest.createRequest(
      builder,
      RequestPayload.LIST_BINDINGS,
      requestOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates a ExitRequest message in the form of a ByteBuffer that can be sent
    * to the debugger.
    *
    * @return the serialized message
    */
  def createSessionExitRequest(): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(64)
    val requestOffset                       = RequestFactory.createSessionExitRequest()
    val outMsg = BinaryRequest.createRequest(
      builder,
      RequestPayload.SESSION_EXIT,
      requestOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates an EvaluationSuccess message in the form of a ByteBuffer that can
    * be sent from the debugger.
    *
    * @param result evaluation result
    * @return the serialized message
    */
  def createEvaluationSuccess(result: Object): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(256)
    val replyOffset                         = ResponseFactory.createEvaluationSuccess(result)
    val outMsg = BinaryResponse.createResponse(
      builder,
      ResponsePayload.EVALUATION_SUCCESS,
      replyOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates an EvaluationFailure message in the form of a ByteBuffer that can
    * be sent from the debugger.
    *
    * @param exception the exception that caused the failure
    * @return the serialized message
    */
  def createEvaluationFailure(exception: Exception): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(256)
    val replyOffset                         = ResponseFactory.createEvaluationFailure(exception)
    val outMsg = BinaryResponse.createResponse(
      builder,
      ResponsePayload.EVALUATION_FAILURE,
      replyOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates a ListBindingsResult message in the form of a ByteBuffer that can
    * be sent from the debugger.
    *
    * @param bindings mapping from names to bound values
    * @return the serialized message
    */
  def createListBindingsResult(bindings: Map[String, Object]): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(256)
    val replyOffset                         = ResponseFactory.createListBindingsResult(bindings)
    val outMsg = BinaryResponse.createResponse(
      builder,
      ResponsePayload.LIST_BINDINGS,
      replyOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates a ListBindingsResult message in the form of a ByteBuffer that can
    * be sent from the debugger.
    * Alternative version that is more friendly to Java code.
    *
    * @param bindings mapping from names to bound values (a Java Map)
    * @return the serialized message
    */
  def createListBindingsResult(
    bindings: java.util.Map[String, Object]
  ): ByteBuffer =
    createListBindingsResult(bindings.asScala.toMap)

  /**
    * Creates an ExitSuccess message in the form of a ByteBuffer that can be
    * sent from the debugger.
    *
    * @return the serialized message
    */
  def createSessionExitSuccess(): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(64)
    val replyOffset                         = ResponseFactory.createSessionExitSuccess()
    val outMsg = BinaryResponse.createResponse(
      builder,
      ResponsePayload.SESSION_EXIT,
      replyOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /**
    * Creates an SessionStartNotification message in the form of a ByteBuffer
    * that can be sent from the debugger.
    *
    * @return the serialized message
    */
  def createSessionStartNotification(): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(64)
    val replyOffset                         = ResponseFactory.createSessionStartNotification()
    val outMsg = BinaryResponse.createResponse(
      builder,
      ResponsePayload.SESSION_START,
      replyOffset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }
}
