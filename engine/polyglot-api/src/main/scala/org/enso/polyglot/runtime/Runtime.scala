package org.enso.polyglot.runtime

import java.io.File
import java.nio.ByteBuffer
import java.util.UUID

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.cbor.CBORFactory
import com.fasterxml.jackson.module.scala.{
  DefaultScalaModule,
  ScalaObjectMapper
}
import org.enso.text.editing.model.TextEdit

import scala.util.Try

object Runtime {

  /**
    * A common supertype for all Runtime API methods.
    */
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
  @JsonSubTypes(
    Array(
      new JsonSubTypes.Type(
        value = classOf[Api.CreateContextRequest],
        name  = "createContextRequest"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.CreateContextResponse],
        name  = "createContextResponse"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.DestroyContextRequest],
        name  = "destroyContextRequest"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.DestroyContextResponse],
        name  = "destroyContextResponse"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.PushContextRequest],
        name  = "pushContextRequest"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.PushContextResponse],
        name  = "pushContextResponse"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.PopContextRequest],
        name  = "popContextRequest"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.PopContextResponse],
        name  = "popContextResponse"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.RecomputeContextRequest],
        name  = "recomputeContextRequest"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.RecomputeContextResponse],
        name  = "recomputeContextResponse"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.OpenFileNotification],
        name  = "openFileNotification"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.EditFileNotification],
        name  = "editFileNotification"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.CloseFileNotification],
        name  = "closeFileNotification"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.ExpressionValuesComputed],
        name  = "expressionValuesComputed"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.ContextNotExistError],
        name  = "contextNotExistError"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.EmptyStackError],
        name  = "emptyStackError"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.InvalidStackItemError],
        name  = "invalidStackItemError"
      ),
      new JsonSubTypes.Type(
        value = classOf[Api.InitializedNotification],
        name  = "initializedNotification"
      )
    )
  )
  sealed trait Api
  sealed trait ApiRequest      extends Api
  sealed trait ApiResponse     extends Api
  sealed trait ApiNotification extends ApiResponse

  object Api {

    type ContextId    = UUID
    type ExpressionId = UUID
    type RequestId    = UUID

    /**
      * Indicates error response.
      */
    sealed trait Error extends ApiResponse

    /**
      * A representation of a pointer to a method definition.
      */
    case class MethodPointer(file: File, definedOnType: String, name: String)

    /**
      * A representation of an executable position in code.
      */
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
    @JsonSubTypes(
      Array(
        new JsonSubTypes.Type(
          value = classOf[StackItem.ExplicitCall],
          name  = "explicitCall"
        ),
        new JsonSubTypes.Type(
          value = classOf[StackItem.LocalCall],
          name  = "localCall"
        )
      )
    )
    sealed trait StackItem

    object StackItem {

      /**
        * A call performed at the top of the stack, to initialize the context.
        */
      case class ExplicitCall(
        methodPointer: MethodPointer,
        thisArgumentExpression: Option[String],
        positionalArgumentsExpressions: Vector[String]
      ) extends StackItem

      /**
        * A call corresponding to "entering a function call".
        */
      case class LocalCall(expressionId: ExpressionId) extends StackItem
    }

    /**
      * An update containing information about expression.
      *
      * @param expressionId expression id.
      * @param expressionType the type of expression.
      * @param shortValue the value of expression.
      * @param methodCall the pointer to a method definition.
      */
    case class ExpressionValueUpdate(
      expressionId: ExpressionId,
      expressionType: Option[String],
      shortValue: Option[String],
      methodCall: Option[MethodPointer]
    )

    /**
      * An object representing invalidated expressions selector.
      */
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
    @JsonSubTypes(
      Array(
        new JsonSubTypes.Type(
          value = classOf[InvalidatedExpressions.All],
          name  = "all"
        ),
        new JsonSubTypes.Type(
          value = classOf[InvalidatedExpressions.Expressions],
          name  = "expressions"
        )
      )
    )
    sealed trait InvalidatedExpressions

    object InvalidatedExpressions {

      /**
        * An object representing invalidation of all expressions.
        */
      case class All() extends InvalidatedExpressions

      /**
        * An object representing invalidation of a list of expressions.
        *
        * @param value a list of expressions to invalidate.
        */
      case class Expressions(value: Vector[ExpressionId]) extends InvalidatedExpressions
    }

    /**
      * A notification about updated expressions of the context.
      *
      * @param contextId the context's id.
      * @param updates a list of updates.
      */
    case class ExpressionValuesComputed(
      contextId: ContextId,
      updates: Vector[ExpressionValueUpdate]
    ) extends ApiNotification

    /**
      * Envelope for an Api request.
      *
      * @param requestId the request identifier.
      * @param payload the request payload.
      */
    case class Request(requestId: Option[RequestId], payload: ApiRequest)

    object Request {

      /**
        * A smart constructor for [[Request]].
        *
        * @param requestId the reqest identifier.
        * @param payload the request payload.
        * @return a request object with specified request id and payload.
        */
      def apply(requestId: RequestId, payload: ApiRequest): Request =
        Request(Some(requestId), payload)

      /**
        * A smart constructor for [[Request]].
        *
        * @param payload the request payload.
        * @return a request object without request id and specified payload.
        */
      def apply(payload: ApiRequest): Request =
        Request(None, payload)
    }

    /**
      * Envelope for an Api response.
      *
      * @param correlationId request that initiated the response
      * @param payload response
      */
    case class Response(correlationId: Option[RequestId], payload: ApiResponse)

    object Response {

      /**
        * A smart constructor for [[Response]].
        *
        * @param correlationId the request id triggering this response.
        * @param payload the response payload.
        * @return a response object with specified correlation id and payload.
        */
      def apply(correlationId: RequestId, payload: ApiResponse): Response =
        Response(Some(correlationId), payload)

      /**
        * A smart constructor for [[Response]] that was not triggered by
        * any request (i.e. a notification).
        *
        * @param payload the data carried by the response.
        * @return a response without a correlation id and specified payload.
        */
      def apply(payload: ApiResponse): Response = Response(None, payload)
    }

    /**
      * A Request sent from the client to the runtime server, to create a new
      * execution context with a given id.
      *
      * @param contextId the newly created context's id.
      */
    case class CreateContextRequest(contextId: ContextId) extends ApiRequest

    /**
      * A response sent from the server upon handling the [[CreateContextRequest]]
      *
      * @param contextId the newly created context's id.
      */
    case class CreateContextResponse(contextId: ContextId) extends ApiResponse

    /**
      * A Request sent from the client to the runtime server, to destroy an
      * execution context with a given id.
      *
      * @param contextId the destroyed context's id.
      */
    case class DestroyContextRequest(contextId: ContextId) extends ApiRequest

    /**
      * A success response sent from the server upon handling the
      * [[DestroyContextRequest]]
      *
      * @param contextId the destroyed context's id
      */
    case class DestroyContextResponse(contextId: ContextId) extends ApiResponse

    /**
      * A Request sent from the client to the runtime server, to move
      * the execution context to a new location deeper down the stack.
      *
      * @param contextId the context's id.
      * @param stackItem an item that should be pushed on the stack.
      */
    case class PushContextRequest(contextId: ContextId, stackItem: StackItem)
        extends ApiRequest

    /**
      * A response sent from the server upon handling the [[PushContextRequest]]
      *
      * @param contextId the context's id.
      */
    case class PushContextResponse(contextId: ContextId) extends ApiResponse

    /**
      * A Request sent from the client to the runtime server, to move
      * the execution context up the stack.
      *
      * @param contextId the context's id.
      */
    case class PopContextRequest(contextId: ContextId) extends ApiRequest

    /**
      * A response sent from the server upon handling the [[PopContextRequest]]
      *
      * @param contextId the context's id.
      */
    case class PopContextResponse(contextId: ContextId) extends ApiResponse

    /**
      * A Request sent from the client to the runtime server, to recompute
      * the execution context.
      *
      * @param contextId the context's id.
      * @param expressions the selector specifying which expressions should be
      * recomputed.
      */
    case class RecomputeContextRequest(
      contextId: ContextId,
      expressions: Option[InvalidatedExpressions]
    ) extends ApiRequest

    /**
      * A response sent from the server upon handling the
      * [[RecomputeContextRequest]]
      *
      * @param contextId the context's id.
      */
    case class RecomputeContextResponse(contextId: ContextId)
        extends ApiResponse

    /**
      * An error response signifying a non-existent context.
      *
      * @param contextId the context's id
      */
    case class ContextNotExistError(contextId: ContextId) extends Error

    /**
      * An error response signifying that stack is empty.
      *
      * @param contextId the context's id
      */
    case class EmptyStackError(contextId: ContextId) extends Error

    /**
      * An error response signifying that stack item is invalid.
      *
      * @param contextId the context's id
      */
    case class InvalidStackItemError(contextId: ContextId) extends Error

    /**
      * A notification sent to the server about switching a file to literal
      * contents.
      *
      * @param path the file being moved to memory.
      * @param contents the current file contents.
      */
    case class OpenFileNotification(path: File, contents: String)
        extends ApiRequest

    /**
      * A notification sent to the server about in-memory file contents being
      * edited.
      *
      * @param path the file being edited.
      * @param edits the diffs to apply to the contents.
      */
    case class EditFileNotification(path: File, edits: Seq[TextEdit])
        extends ApiRequest

    /**
      * A notification sent to the server about dropping the file from memory
      * back to on-disk version.
      *
      * @param path the file being closed.
      */
    case class CloseFileNotification(path: File) extends ApiRequest

    /**
      * Notification sent from the server to the client upon successful
      * initialization. Any messages sent to the server before receiving this
      * message will be dropped.
      */
    case class InitializedNotification() extends ApiResponse

    private lazy val mapper = {
      val factory = new CBORFactory()
      val mapper  = new ObjectMapper(factory) with ScalaObjectMapper
      mapper.registerModule(DefaultScalaModule)
    }

    /**
      * Serializes a Request into a byte buffer.
      *
      * @param message the message to serialize.
      * @return the serialized version of the message.
      */
    def serialize(message: Request): ByteBuffer =
      ByteBuffer.wrap(mapper.writeValueAsBytes(message))

    /**
      * Serializes a Response into a byte buffer.
      *
      * @param message the message to serialize.
      * @return the serialized version of the message.
      */
    def serialize(message: Response): ByteBuffer =
      ByteBuffer.wrap(mapper.writeValueAsBytes(message))

    /**
      * Deserializes a byte buffer into a Request message.
      *
      * @param bytes the buffer to deserialize
      * @return the deserialized message, if the byte buffer can be deserialized.
      */
    def deserializeRequest(bytes: ByteBuffer): Option[Request] =
      Try(mapper.readValue(bytes.array(), classOf[Request])).toOption

    /**
      * Deserializes a byte buffer into a Response message.
      *
      * @param bytes the buffer to deserialize
      * @return the deserialized message, if the byte buffer can be deserialized.
      */
    def deserializeResponse(bytes: ByteBuffer): Option[Response] =
      Try(mapper.readValue(bytes.array(), classOf[Response])).toOption
  }

}
