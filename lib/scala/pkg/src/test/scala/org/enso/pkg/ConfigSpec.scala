package org.enso.pkg

import org.enso.semver.SemVer
import org.enso.editions.LibraryName
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Inside, OptionValues}
import org.yaml.snakeyaml.error.YAMLException

import scala.util.Failure

class ConfigSpec
    extends AnyWordSpec
    with Matchers
    with Inside
    with OptionValues {

  "Config" should {
    "deserialize the serialized representation to the original value" in {
      val config = Config(
        name           = "placeholder",
        normalizedName = None,
        version        = "dev",
        namespace      = "local",
        edition =
          Some(Config.makeCompatibilityEditionFromVersion(SemVer.of(4, 5, 6))),
        license = "none",
        authors = List(),
        maintainers = List(
          Contact(Some("A"), Some("a@example.com")),
          Contact(Some("B"), None),
          Contact(None, Some("c@example.com"))
        ),
        preferLocalLibraries = true,
        componentGroups      = None,
        requires             = List()
      )
      val deserialized = Config.fromYaml(config.toYaml).get
      deserialized shouldEqual config
    }

    "only require the name and use defaults for everything else" in {
      val parsed = Config.fromYaml("name: fooBar").get
      parsed.name shouldEqual "fooBar"
      parsed.normalizedName shouldEqual None
      parsed.moduleName shouldEqual "FooBar"
      parsed.edition shouldBe empty
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

    "correctly parse empty edition field" in {
      val config =
        """name: FooBar
          |edition:
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      parsed.edition shouldBe None
    }
  }

  "Component groups" should {

    "correctly de-serialize and serialize back the components syntax" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |    - Group 1:
          |        color: '#C047AB'
          |        icon: icon-name
          |        exports:
          |          - foo:
          |              shortcut: f
          |          - bar
          |  extends:
          |    - Standard.Base.Group 2:
          |        exports:
          |          - baz:
          |              shortcut: k
          |          - quux
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      val expectedComponentGroups = ComponentGroups(
        newGroups = List(
          ComponentGroup(
            group = GroupName("Group 1"),
            color = Some("#C047AB"),
            icon  = Some("icon-name"),
            exports = List(
              Component("foo", Some(Shortcut("f"))),
              Component("bar", None)
            )
          )
        ),
        extendedGroups = List(
          ExtendedComponentGroup(
            group = GroupReference(
              LibraryName("Standard", "Base"),
              GroupName("Group 2")
            ),
            exports = List(
              Component("baz", Some(Shortcut("k"))),
              Component("quux", None)
            )
          )
        )
      )
      parsed.componentGroups shouldEqual Some(expectedComponentGroups)

      val serialized = parsed.toYaml
      serialized should include(
        """component-groups:
          |  new:
          |  - Group 1:
          |      color: '#C047AB'
          |      icon: icon-name
          |      exports:
          |      - foo:
          |          shortcut: f
          |      - bar
          |  extends:
          |  - Standard.Base.Group 2:
          |      exports:
          |      - baz:
          |          shortcut: k
          |      - quux""".stripMargin.linesIterator.mkString("\n")
      )
    }

    "correctly de-serialize empty components" in {
      val config =
        """name: FooBar
          |component-groups:
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      parsed.componentGroups shouldEqual None
    }

    "allow unknown keys in component groups" in {
      val config =
        """name: FooBar
          |component-groups:
          |  foo:
          |  - Group 1:
          |    exports:
          |    - bax
          |""".stripMargin
      val parsed = Config.fromYaml(config).get

      parsed.componentGroups shouldEqual Some(ComponentGroups.empty)
    }

    "fail to de-serialize invalid extended modules" in {
      val config =
        """name: FooBar
          |component-groups:
          |  extends:
          |  - Group 1:
          |      exports:
          |      - bax
          |""".stripMargin
      val parsed = Config.fromYaml(config)

      parsed match {
        case Failure(failure: YAMLException) =>
          failure.getMessage should include(
            "Failed to decode 'Group 1' as a module reference"
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
          |      exports:
          |      - foo:
          |          shortcut: f
          |      - bar:
          |          shortcut: fgh
          |      - baz:
          |          shortcut: 0
          |      - quux:
          |          shortcut:
          |      - hmmm
          |""".stripMargin
      val parsed = Config.fromYaml(config).get
      val expectedComponentGroups = ComponentGroups(
        newGroups = List(
          ComponentGroup(
            group = GroupName("Group 1"),
            color = None,
            icon  = None,
            exports = List(
              Component("foo", Some(Shortcut("f"))),
              Component("bar", Some(Shortcut("fgh"))),
              Component("baz", Some(Shortcut("0"))),
              Component("quux", None),
              Component("hmmm", None)
            )
          )
        ),
        extendedGroups = List()
      )

      parsed.componentGroups shouldEqual Some(expectedComponentGroups)
    }

    "fail to de-serialize invalid shortcuts" in {
      val config =
        """name: FooBar
          |component-groups:
          |  new:
          |  - Group 1:
          |      exports:
          |      - foo:
          |          shortcut: []
          |""".stripMargin
      val parsed = Config.fromYaml(config)
      parsed match {
        case Failure(failure: YAMLException) =>
          failure.getMessage should equal(
            "Failed to decode shortcut. Expected a string value, got a sequence"
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
      val parsed = Config.fromYaml(config)
      parsed match {
        case Failure(failure: YAMLException) =>
          failure.getMessage should equal(
            "Failed to decode component group. Expected a mapping, got a sequence"
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
          |      exports:
          |      - one: two
          |""".stripMargin
      val parsed = Config.fromYaml(config)
      parsed match {
        case Failure(failure: YAMLException) =>
          failure.getMessage should equal(
            "Failed to decode exported component 'one'"
          )
        case unexpected =>
          fail(s"Unexpected result: $unexpected")
      }
    }
  }

}
