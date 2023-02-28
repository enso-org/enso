package org.enso.loggingservice.internal

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import java.time.temporal.ChronoUnit

class DefaultLogMessageRendererSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues {

  "DefaultLogMessageRenderer" should {
    "render NullPointerException" in {
      val renderer = new DefaultLogMessageRenderer(printExceptions = true)
      val ts       = Instant.now().truncatedTo(ChronoUnit.MILLIS)

      val exception =
        SerializedException.fromException(new NullPointerException)
      val message =
        WSLogMessage(LogLevel.Trace, ts, "group", "message", Some(exception))

      noException should be thrownBy renderer.render(message)
    }

    "render NullPointerException with null message" in {
      val renderer = new DefaultLogMessageRenderer(printExceptions = true)
      val ts       = Instant.now().truncatedTo(ChronoUnit.MILLIS)

      val exception =
        SerializedException.fromException(new NullPointerException)
      val message =
        WSLogMessage(LogLevel.Trace, ts, null, null, Some(exception))

      val txt = renderer.render(message)
      txt.toString() should equal(txt)
    }

    "JSONize NullPointerException with null message" in {
      val ts = Instant.now().truncatedTo(ChronoUnit.MILLIS)

      val exception =
        SerializedException.fromException(new NullPointerException)
      val message =
        WSLogMessage(LogLevel.Trace, ts, null, null, Some(exception))

      import io.circe.syntax._

      message.asJson.noSpaces
    }
  }
}
