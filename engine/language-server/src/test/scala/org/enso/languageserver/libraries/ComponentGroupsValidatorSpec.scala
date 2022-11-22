package org.enso.languageserver.libraries

import io.circe.DecodingFailure
import org.enso.editions.LibraryName
import org.enso.pkg.{ComponentGroups, Config, GroupName, GroupReference}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentGroupsValidatorSpec extends AnyWordSpec with Matchers {

  import ComponentGroupsValidator._
  import ComponentGroupsValidatorSpec._
  import ComponentGroupsResolverSpec._

  "ComponentGroupsValidator" should {

    "validate invalid component groups" in {
      val validator = new ComponentGroupsValidator
      val testPackages = Vector(
        config("Foo", "Bar"),
        config(
          "Foo",
          "Baz",
          ComponentGroups(List(newComponentGroup("Mod1", "one", "two")), List())
        ),
        configError(
          "Foo",
          "Quux",
          "Error message"
        )
      )
      val expected = testPackages.map { config =>
        val groups = config.componentGroups.left.map { error =>
          ValidationError.InvalidComponentGroups(
            libraryName(config),
            error.getMessage()
          )
        }
        libraryName(config) -> groups
      }

      validator.validate(testPackages) shouldEqual expected
    }

    "validate duplicate component groups" in {
      val validator = new ComponentGroupsValidator
      val testPackages = Vector(
        config(
          "Foo",
          "Bar",
          ComponentGroups(List(newComponentGroup("Mod1", "a", "b")), List())
        ),
        config(
          "Foo",
          "Bar",
          ComponentGroups(List(newComponentGroup("Mod1", "one", "two")), List())
        ),
        config(
          "Baz",
          "Quux",
          ComponentGroups(List(newComponentGroup("Mod1", "one", "two")), List())
        )
      )
      val expected = Vector(
        libraryName(testPackages(0)) -> testPackages(0).componentGroups,
        libraryName(testPackages(1)) -> Left(
          ValidationError.DuplicatedComponentGroup(
            libraryName(testPackages(1)),
            GroupReference(LibraryName("Foo", "Bar"), GroupName("Mod1"))
          )
        ),
        libraryName(testPackages(2)) -> testPackages(2).componentGroups
      )

      validator.validate(testPackages) shouldEqual expected
    }

    "validate non-existent extensions" in {
      val validator = new ComponentGroupsValidator
      val testPackages = Vector(
        config(
          "Foo",
          "Bar",
          ComponentGroups(List(newComponentGroup("Mod1", "a", "b")), List())
        ),
        config(
          "Foo",
          "Baz",
          ComponentGroups(
            List(),
            List(extendedComponentGroup("Foo", "Bar", "Mod1", "c", "d"))
          )
        ),
        config(
          "Baz",
          "Quux",
          ComponentGroups(
            List(),
            List(extendedComponentGroup("Foo", "Baz", "Mod1", "quuux"))
          )
        )
      )
      val expected = Vector(
        libraryName(testPackages(0)) -> testPackages(0).componentGroups,
        libraryName(testPackages(1)) -> testPackages(1).componentGroups,
        libraryName(testPackages(2)) -> Left(
          ValidationError.ComponentGroupExtendsNothing(
            libraryName(testPackages(2)),
            GroupReference(LibraryName("Foo", "Baz"), GroupName("Mod1"))
          )
        )
      )

      validator.validate(testPackages) shouldEqual expected
    }

    "preserve component groups order" in {
      val validator = new ComponentGroupsValidator
      val testPackages = Vector(
        config(
          "Foo",
          "Bar",
          ComponentGroups(List(newComponentGroup("Mod1", "one", "two")), List())
        ),
        config(
          "Foo",
          "Baz",
          ComponentGroups(List(newComponentGroup("Mod2", "one", "two")), List())
        ),
        config(
          "Foo",
          "Abc",
          ComponentGroups(
            List(newComponentGroup("Abc1", "four", "five")),
            List()
          )
        )
      )
      val expected = testPackages.map { config =>
        libraryName(config) -> config.componentGroups
      }

      validator.validate(testPackages) shouldEqual expected
    }

  }

}

object ComponentGroupsValidatorSpec {

  /** Create a new config with containing a component groups error. */
  def configError(
    namespace: String,
    name: String,
    message: String
  ): Config =
    Config(
      name                 = name,
      namespace            = namespace,
      version              = "0.0.1",
      license              = "",
      authors              = Nil,
      maintainers          = Nil,
      edition              = None,
      preferLocalLibraries = true,
      componentGroups      = Left(DecodingFailure.apply(message, List()))
    )

  /** Create a library name from config. */
  def libraryName(config: Config): LibraryName =
    LibraryName(config.namespace, config.name)
}
