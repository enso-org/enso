package org.enso.loggingservice.internal

import io.circe.syntax._
import org.enso.loggingservice.internal.protocol.SerializedException
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SerializedExceptionSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues {

  "SerializedException" should {
    "serialize and deserialize with nested causes" in {
      val cause = SerializedException(
        "cause",
        "msg",
        Seq(SerializedException.TraceElement("e1", "loc1"))
      )

      val exception = SerializedException(
        "root",
        "msg2",
        Seq(
          SerializedException.TraceElement("e2", "loc2"),
          SerializedException.TraceElement("e3", "loc3")
        ),
        cause
      )

      exception.asJson
        .as[SerializedException]
        .toOption
        .value shouldEqual exception
    }

    "be created from NullPointerException" in {
      val exception = new NullPointerException()
      val result    = SerializedException.fromException(exception)

      result.name shouldEqual exception.getClass.getName
      result.message shouldEqual None
      result.cause shouldEqual None
      result.stackTrace shouldBe Symbol("nonEmpty")
    }
  }
}
