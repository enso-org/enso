package org.enso.pkg

import cats.Show
import io.circe.{DecodingFailure, Json, JsonObject}
import nl.gn0s1s.bump.SemVer
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Inside, OptionValues}

import scala.util.Failure

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
        name      = "placeholder",
        version   = "dev",
        namespace = "local",
        edition =
          Some(Config.makeCompatibilityEditionFromVersion(SemVer(4, 5, 6))),
        license = "none",
        authors = List(),
        maintainers = List(
          Contact(Some("A"), Some("a@example.com")),
          Contact(Some("B"), None),
          Contact(None, Some("c@example.com"))
        ),
        preferLocalLibraries = true,
        componentGroups      = ComponentGroups.empty
      )
      val deserialized = Config.fromYaml(config.toYaml).get
      val withoutJson  = deserialized.copy(originalJson = JsonObject())
      withoutJson shouldEqual config
    }

    "only require the name and use defaults for everything else" in {
      val parsed = Config.fromYaml("name: FooBar").get
      parsed.name shouldEqual "FooBar"
      parsed.edition shouldBe empty
    }

    "be backwards compatible but correctly migrate to new format on save" in {
      val oldFormat =
        """name: FooBar
          |enso-version: 1.2.3
          |extra-key: extra-value
          |""".stripMargin
      val parsed = Config.fromYaml(oldFormat).get

      parsed.edition.get.engineVersion should contain(SemVer(1, 2, 3))

      val serialized  = parsed.toYaml
      val parsedAgain = Config.fromYaml(serialized).get

      parsedAgain.copy(originalJson = JsonObject()) shouldEqual
      parsed.copy(originalJson      = JsonObject())

      parsedAgain.originalJson("extra-key").flatMap(_.asString) should contain(
        "extra-value"
      )
    }

    "correctly de-serialize and serialize back the shortened edition syntax " +
    "if there are no overrides" in {
      val config =
        """name: FooBar
          |edition: 2020.1
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      parsed.edition.get.parent should contain("2020.1")

      val serialized = parsed.toYaml
      serialized should include("edition: '2020.1'")
    }
  }

  "Component groups" should {

    "correctly de-serialize and serialize back the components syntax" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |  - Group 1:
          |    color: '#C047AB'
          |    icon: icon-name
          |    exports:
          |      - foo:
          |        shortcut: f
          |      - bar
          |  extends:
          |  - Base.Group 2:
          |    exports:
          |      - bax
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      val expectedComponentGroups = ComponentGroups(
        `new` = List(
          ComponentGroup(
            module = "Group 1",
            color  = Some("#C047AB"),
            icon   = Some("icon-name"),
            exports = List(
              Component("foo", Some(Shortcut("f"))),
              Component("bar", None)
            )
          )
        ),
        `extends` = List(
          ComponentGroup(
            module  = "Base.Group 2",
            color   = None,
            icon    = None,
            exports = List(Component("bax", None))
          )
        )
      )
      parsed.componentGroups shouldEqual expectedComponentGroups

      val serialized = parsed.toYaml
      serialized should include("""component-groups:
                                  |  new:
                                  |  - module: Group 1
                                  |    color: '#C047AB'
                                  |    icon: icon-name
                                  |    exports:
                                  |    - name: foo
                                  |      shortcut: f
                                  |    - bar
                                  |  extends:
                                  |  - module: Base.Group 2
                                  |    exports:
                                  |    - bax""".stripMargin)
    }

    "correctly de-serialize empty components" in {
      val config =
        """name: FooBar
          |component-groups:
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      parsed.componentGroups shouldEqual ComponentGroups.empty
    }

    "fail to de-serialize unknown keys in component groups" in {
      val config =
        """name: FooBar
          |component-groups:
          |  foo:
          |  - Group 1:
          |    exports:
          |    - bax
          |""".stripMargin

      Config.fromYaml(config) match {
        case Failure(f: DecodingFailure) =>
          Show[DecodingFailure].show(f) should include(
            "Invalid component group"
          )
        case unexpected =>
          fail(s"Unexpected result: $unexpected")
      }
    }

    "correctly de-serialize shortcuts" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |  - Group 1:
          |    exports:
          |    - foo:
          |      shortcut: f
          |    - bar:
          |      shortcut: fgh
          |    - baz:
          |      shortcut: 0
          |    - quux:
          |      shortcut:
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      parsed.componentGroups shouldEqual ComponentGroups(
        `new` = List(
          ComponentGroup(
            module = "Group 1",
            color  = None,
            icon   = None,
            exports = List(
              Component("foo", Some(Shortcut("f"))),
              Component("bar", Some(Shortcut("fgh"))),
              Component("baz", Some(Shortcut("0"))),
              Component("quux", None)
            )
          )
        ),
        `extends` = List()
      )
    }

    "fail ot de-serialize invalid shortcuts" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |  - Group 1:
          |    exports:
          |    - foo:
          |      shortcut: []
          |""".stripMargin
      Config.fromYaml(config) match {
        case Failure(f: DecodingFailure) =>
          Show[DecodingFailure].show(f) should include(
            "Failed to decode shortcut"
          )
        case unexpected =>
          fail(s"Unexpected result: $unexpected")
      }
    }

    "fail to de-serialize invalid component groups" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |  - exports:
          |    - name: foo
          |""".stripMargin

      Config.fromYaml(config) match {
        case Failure(f: DecodingFailure) =>
          Show[DecodingFailure].show(f) should include(
            "Failed to decode component group name"
          )
        case unexpected =>
          fail(s"Unexpected result: $unexpected")
      }
    }

    "fail to de-serialize invalid components" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |  - Group 1:
          |    exports:
          |    - one: two
          |""".stripMargin

      Config.fromYaml(config) match {
        case Failure(f: DecodingFailure) =>
          Show[DecodingFailure].show(f) should include(
            "Failed to decode component name"
          )
        case unexpected =>
          fail(s"Unexpected result: $unexpected")
      }
    }
  }

}
