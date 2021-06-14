package org.enso.pkg

import io.circe.{Json, JsonObject}
import nl.gn0s1s.bump.SemVer
import org.enso.editions.{DefaultEnsoVersion, SemVerEnsoVersion}
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

      inside(original.as[Config]) { case Right(config) =>
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
        name    = "placeholder",
        version = "dev",
        edition = Config.makeCompatibilityEditionFromVersion(
          SemVerEnsoVersion(SemVer(4, 5, 6))
        ),
        license = "none",
        authors = List(),
        maintainers = List(
          Contact(Some("A"), Some("a@example.com")),
          Contact(Some("B"), None),
          Contact(None, Some("c@example.com"))
        ),
        preferLocalLibraries = true
      )
      val deserialized = Config.fromYaml(config.toYaml).get
      val withoutJson  = deserialized.copy(originalJson = JsonObject())
      withoutJson shouldEqual config
    }

    "only require the name and use defaults for everything else" in {
      val parsed = Config.fromYaml("name: FooBar").get
      parsed.name shouldEqual "FooBar"
      parsed.edition.engineVersion should contain(DefaultEnsoVersion)
    }

    "be backwards compatible but correctly migrate to new format on save" in {
      val oldFormat =
        """name: FooBar
          |enso-version: 1.2.3
          |extra-key: extra-value
          |""".stripMargin
      val parsed = Config.fromYaml(oldFormat).get

      parsed.edition.engineVersion should contain(
        SemVerEnsoVersion(SemVer(1, 2, 3))
      )

      val serialized  = parsed.toYaml
      val parsedAgain = Config.fromYaml(serialized).get

      parsedAgain.copy(originalJson = JsonObject()) shouldEqual
      parsed.copy(originalJson      = JsonObject())

      parsedAgain.originalJson("extra-key").flatMap(_.asString) should contain(
        "extra-value"
      )
    }
  }
}
