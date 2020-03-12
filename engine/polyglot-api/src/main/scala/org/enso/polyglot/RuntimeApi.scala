package org.enso.polyglot

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

/**
  * A common supertype for all Runtime API methods.
  */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[RuntimeApi.CreateContextRequest],
      name  = "createContextRequest"
    ),
    new JsonSubTypes.Type(
      value = classOf[RuntimeApi.CreateContextResponse],
      name  = "createContextResponse"
    )
  )
)
sealed trait RuntimeApi

object RuntimeApi {
  type ContextId = UUID

  /**
    * A Request sent from the client to the runtime server, to create a new
    * execution context with a given id.
    *
    * @param id the newly created context's id.
    */
  case class CreateContextRequest(id: ContextId) extends RuntimeApi

  /**
    * A response sent from the server upon handling the [[CreateContextRequest]]
    *
    * @param id the newly created context's id.
    */
  case class CreateContextResponse(id: ContextId) extends RuntimeApi

  private lazy val factory = new CBORFactory()
  private lazy val mapper = {
    val mapper = new ObjectMapper(factory) with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
  }

  /**
    * Serializes a protocol message into a byte buffer.
    *
    * @param message the message to serialize.
    * @return the serialized version of the message.
    */
  def serialize(message: RuntimeApi): ByteBuffer =
    ByteBuffer.wrap(mapper.writeValueAsBytes(message))

  /**
    * Deserializes a byte buffer into a protocol message.
    *
    * @param bytes the buffer to deserialize
    * @return the deserialized message, if the byte buffer can be deserialized.
    */
  def deserialize(bytes: ByteBuffer): Option[RuntimeApi] =
    Try(mapper.readValue(bytes.array(), classOf[RuntimeApi])).toOption
}
