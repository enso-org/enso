package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.pkg.{
  ComponentGroup,
  ComponentGroups,
  Config,
  GroupName,
  GroupReference
}
import org.enso.testkit.ReportLogsOnFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentGroupsValidatorSpec
    extends AnyWordSpec
    with Matchers
    with ReportLogsOnFailure {

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
        duplicateGroupsConfigError(
          "Foo",
          "Quux",
          "Foo"
        )
      )
      val expected = Vector(
        libraryName(testPackages(0)) -> Right(
          testPackages(0).componentGroups.getOrElse(ComponentGroups.empty)
        ),
        libraryName(testPackages(1)) -> Right(
          testPackages(1).componentGroups.getOrElse(ComponentGroups.empty)
        ),
        libraryName(testPackages(2)) -> Left(
          ValidationError.DuplicatedComponentGroup(
            libraryName(testPackages(2)),
            GroupReference(LibraryName("Foo", "Quux"), GroupName("Foo"))
          )
        )
      )

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
        libraryName(testPackages(0)) -> Right(
          testPackages(0).componentGroups.getOrElse(ComponentGroups.empty)
        ),
        libraryName(testPackages(1)) -> Left(
          ValidationError.DuplicatedComponentGroup(
            libraryName(testPackages(1)),
            GroupReference(LibraryName("Foo", "Bar"), GroupName("Mod1"))
          )
        ),
        libraryName(testPackages(2)) -> Right(
          testPackages(2).componentGroups.getOrElse(ComponentGroups.empty)
        )
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
        libraryName(testPackages(0)) -> Right(
          testPackages(0).componentGroups.getOrElse(ComponentGroups.empty)
        ),
        libraryName(testPackages(1)) -> Right(
          testPackages(1).componentGroups.getOrElse(ComponentGroups.empty)
        ),
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
        libraryName(config) -> Right(
          config.componentGroups.getOrElse(ComponentGroups.empty)
        )
      }

      validator.validate(testPackages) shouldEqual expected
    }

  }

}

object ComponentGroupsValidatorSpec {

  /** Create a new config with containing a component groups error. */
  def duplicateGroupsConfigError(
    namespace: String,
    name: String,
    groupName: String
  ): Config =
    Config(
      name                 = name,
      normalizedName       = None,
      namespace            = namespace,
      version              = "0.0.1",
      license              = "",
      authors              = Nil,
      maintainers          = Nil,
      edition              = None,
      preferLocalLibraries = true,
      componentGroups = Some(
        ComponentGroups(
          newGroups = List(
            ComponentGroup(
              group   = GroupName(groupName),
              color   = None,
              icon    = None,
              exports = Seq.empty
            ),
            ComponentGroup(
              group   = GroupName(groupName),
              color   = None,
              icon    = None,
              exports = Seq.empty
            )
          ),
          extendedGroups = Nil
        )
      )
    )

  /** Create a library name from config. */
  def libraryName(config: Config): LibraryName =
    LibraryName(config.namespace, config.moduleName)
}
