package org.enso.loggingservice.internal

import java.time.Instant
import java.time.temporal.ChronoUnit

import io.circe.syntax._
import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WSLogMessageSpec extends AnyWordSpec with Matchers with OptionValues {
  "WSLogMessage" should {
    "serialize and deserialize to the same thing" in {
      val ts = Instant.now().truncatedTo(ChronoUnit.MILLIS)

      val message1 = WSLogMessage(LogLevel.Trace, ts, "group", "message", None)
      message1.asJson.as[WSLogMessage].toOption.value shouldEqual message1

      val exception = SerializedException("name", "message", Seq(), None)
      val message2 =
        WSLogMessage(LogLevel.Trace, ts, "group", "message", Some(exception))
      message2.asJson.as[WSLogMessage].toOption.value shouldEqual message2
    }
  }
}
