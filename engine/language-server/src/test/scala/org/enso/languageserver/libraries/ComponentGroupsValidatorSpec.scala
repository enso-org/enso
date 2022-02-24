package org.enso.languageserver.libraries

import io.circe.DecodingFailure
import org.enso.editions.LibraryName
import org.enso.pkg.{ComponentGroups, Config, ModuleName, ModuleReference}
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

      validator.validate(testPackages) shouldEqual testPackages.map { config =>
        config.componentGroups match {
          case Right(_) =>
            Right(config)
          case Left(error) =>
            Left(
              ValidationError.InvalidComponentGroups(
                libraryName(config),
                error.getMessage()
              )
            )
        }
      }
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

      validator
        .validate(testPackages) shouldEqual Vector(
        Right(testPackages(0)),
        Left(
          ValidationError.DuplicatedComponentGroup(
            libraryName(testPackages(1)),
            ModuleReference(LibraryName("Foo", "Bar"), ModuleName("Mod1"))
          )
        ),
        Right(testPackages(2))
      )
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

      validator
        .validate(testPackages) shouldEqual Vector(
        Right(testPackages(0)),
        Right(testPackages(1)),
        Left(
          ValidationError.ComponentGroupExtendsNothing(
            libraryName(testPackages(2)),
            ModuleReference(LibraryName("Foo", "Baz"), ModuleName("Mod1"))
          )
        )
      )
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
