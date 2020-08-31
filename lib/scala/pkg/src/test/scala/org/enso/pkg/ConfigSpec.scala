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

    "deserialize the serialized representation to the original value" in {
      val config = Config(
        name        = "placeholder",
        version     = "dev",
        ensoVersion = DefaultEnsoVersion,
        license     = "none",
        author      = List(),
        maintainer =
          List(Contact("A", Some("a@example.com")), Contact("B", None)),
        dependencies = List(Dependency("dependency", "0.0.0"))
      )
      val deserialized = Config.fromYaml(config.toYaml).get
      val withoutJson  = deserialized.copy(originalJson = Json.obj())
      withoutJson shouldEqual config
    }
  }

  "Contact" should {
    "be parsed correctly" in {
      val configNone =
        """
          |name: placeholder
          |author:
          |""".stripMargin
      Config.fromYaml(configNone).get.author shouldEqual List()
      val configSingle =
        """
          |name: placeholder
          |author: Single Author
          |""".stripMargin
      Config.fromYaml(configSingle).get.author shouldEqual List(
        Contact("Single Author", None)
      )
      val configMulti =
        """
          |name: placeholder
          |author:
          | - Author Number One <one@example.com>
          | - Author Number Two
          |""".stripMargin
      Config.fromYaml(configMulti).get.author shouldEqual List(
        Contact("Author Number One", Some("one@example.com")),
        Contact("Author Number Two", None)
      )
    }
  }
}
