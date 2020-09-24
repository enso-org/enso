package org.enso.loggingservice.internal

import java.sql.Timestamp
import java.time.Instant

import io.circe.syntax._
import org.enso.loggingservice.LogLevel
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WSLogMessageSpec extends AnyWordSpec with Matchers with OptionValues {
  "WSLogMessage" should {
    "serialize and deserialize to the same thing" in {
      val ts = Timestamp.from(Instant.now())
      // we sent time in ms, so nanos are skipped by serialization
      ts.setNanos(0)
      val message1 = WSLogMessage(LogLevel.Trace, ts, "group", "message", None)

      message1.asJson.as[WSLogMessage].toOption.value shouldEqual message1

      val exception = SerializedException("name", "message", Seq(), None)
      val message2 =
        WSLogMessage(LogLevel.Trace, ts, "group", "message", Some(exception))

      message2.asJson.as[WSLogMessage].toOption.value shouldEqual message2
    }
  }
}
