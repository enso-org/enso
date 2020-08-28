package org.enso.pkg

import io.circe.Json
import org.scalatest.{Inside, OptionValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigSpec
    extends AnyWordSpec
    with Matchers
    with Inside
    with OptionValues {
  "Config" should {
    "preserve unknown keys when deserialized and serialized again" in {
      val original = Json.obj(
        "name"        -> Json.fromString("name"),
        "unknown-key" -> Json.fromString("value")
      )

      inside(original.as[Config]) {
        case Right(config) =>
          val serialized = Config.encoder(config)
          serialized.asObject
            .value("unknown-key")
            .value
            .asString
            .value shouldEqual "value"
      }
    }
  }
}
