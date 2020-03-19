package org.enso.polyglot.runtime

import java.nio.ByteBuffer
import java.util.UUID

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.cbor.CBORFactory
import com.fasterxml.jackson.module.scala.{
  DefaultScalaModule,
  ScalaObjectMapper
}

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
      )
    )
  )
  sealed trait Api

  object Api {
    type ContextId = UUID

    /**
      * A Request sent from the client to the runtime server, to create a new
      * execution context with a given id.
      *
      * @param id the newly created context's id.
      */
    case class CreateContextRequest(id: ContextId) extends Api

    /**
      * A response sent from the server upon handling the [[CreateContextRequest]]
      *
      * @param id the newly created context's id.
      */
    case class CreateContextResponse(id: ContextId) extends Api

    /**
      * A Request sent from the client to the runtime server, to destroy an
      * execution context with a given id.
      *
      * @param id the destroyed context's id.
      *
      */
    case class DestroyContextRequest(id: ContextId) extends Api

    /**
      * A success response sent from the server upon handling the
      * [[DestroyContextRequest]]
      *
      * @param id the destroyed context's id.
      */
    case class DestroyContextResponse(
      id: ContextId,
      error: Option[ContextDoesNotExistError]
    ) extends Api

    /**
      * An error payload signifying a non-existent context.
      */
    case class ContextDoesNotExistError()

    private lazy val mapper = {
      val factory = new CBORFactory()
      val mapper = new ObjectMapper(factory) with ScalaObjectMapper
      mapper.registerModule(DefaultScalaModule)
    }

    /**
      * Serializes a protocol message into a byte buffer.
      *
      * @param message the message to serialize.
      * @return the serialized version of the message.
      */
    def serialize(message: Api): ByteBuffer =
      ByteBuffer.wrap(mapper.writeValueAsBytes(message))

    /**
      * Deserializes a byte buffer into a protocol message.
      *
      * @param bytes the buffer to deserialize
      * @return the deserialized message, if the byte buffer can be deserialized.
      */
    def deserialize(bytes: ByteBuffer): Option[Api] =
      Try(mapper.readValue(bytes.array(), classOf[Api])).toOption
  }

}
