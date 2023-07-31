package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.pkg.{
  Component,
  ComponentGroup,
  ComponentGroups,
  Config,
  ExtendedComponentGroup,
  GroupName,
  GroupReference
}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentGroupsResolverSpec extends AnyWordSpec with Matchers {

  import ComponentGroupsResolverSpec._

  "ComponentGroupsResolver" should {

    "return a list of defined component groups preserving the order" in {
      val resolver = new ComponentGroupsResolver
      val testPackages = Vector(
        config("Foo", "Bar"),
        config(
          "Foo",
          "Baz",
          ComponentGroups(List(newComponentGroup("Mod1", "one", "two")), List())
        ),
        config(
          "Foo",
          "Quux",
          ComponentGroups(List(newComponentGroup("Mod2", "one")), List())
        ),
        config(
          "Foo",
          "Brrr",
          ComponentGroups(List(newComponentGroup("Abc", "three")), List())
        )
      )

      resolver.run(testPackages) shouldEqual Vector(
        libraryComponentGroup("Foo", "Baz", "Mod1", "one", "two"),
        libraryComponentGroup("Foo", "Quux", "Mod2", "one"),
        libraryComponentGroup("Foo", "Brrr", "Abc", "three")
      )
    }

    "drop duplicated library definitions" in {
      val resolver = new ComponentGroupsResolver
      val testPackages = Vector(
        config("Foo", "Bar"),
        config(
          "Foo",
          "Baz",
          ComponentGroups(
            List(newComponentGroup("Mod1", "one", "two")),
            List()
          )
        ),
        config(
          "Foo",
          "Baz",
          ComponentGroups(List(newComponentGroup("Mod1", "one")), List())
        ),
        config(
          "Foo",
          "Quux",
          ComponentGroups(List(newComponentGroup("Mod2", "one")), List())
        )
      )

      resolver.run(testPackages) shouldEqual Vector(
        libraryComponentGroup("Foo", "Baz", "Mod1", "one", "two"),
        libraryComponentGroup("Foo", "Quux", "Mod2", "one")
      )
    }

    "drop duplicated component group definitions" in {
      val resolver = new ComponentGroupsResolver
      val testPackages = Vector(
        config("Foo", "Bar"),
        config(
          "Foo",
          "Baz",
          ComponentGroups(
            List(
              newComponentGroup("Mod1", "one", "two"),
              newComponentGroup("Mod1", "three")
            ),
            List()
          )
        ),
        config(
          "Foo",
          "Quux",
          ComponentGroups(List(newComponentGroup("Mod2", "one")), List())
        )
      )

      resolver.run(testPackages) shouldEqual Vector(
        libraryComponentGroup("Foo", "Baz", "Mod1", "one", "two"),
        libraryComponentGroup("Foo", "Quux", "Mod2", "one")
      )
    }

    "apply extended component groups" in {
      val resolver = new ComponentGroupsResolver
      val testPackages = Vector(
        config(
          "user",
          "Unnamed",
          ComponentGroups(
            List(newComponentGroup("Main", "main")),
            List(
              extendedComponentGroup("Standard", "Base", "Data.Vector", "quux")
            )
          )
        ),
        config(
          "Standard",
          "Base",
          ComponentGroups(
            List(newComponentGroup("Data.Vector", "one", "two")),
            List()
          )
        ),
        config(
          "user",
          "Vector_Utils",
          ComponentGroups(
            List(),
            List(
              extendedComponentGroup("Standard", "Base", "Data.Vector", "three")
            )
          )
        )
      )

      resolver.run(testPackages) shouldEqual Vector(
        libraryComponentGroup("user", "Unnamed", "Main", "main"),
        libraryComponentGroup(
          "Standard",
          "Base",
          "Data.Vector",
          "one",
          "two",
          "quux",
          "three"
        )
      )
    }

    "apply mutually extended component groups" in {
      val resolver = new ComponentGroupsResolver
      val testPackages = Vector(
        config(
          "Standard",
          "Table",
          ComponentGroups(
            List(newComponentGroup("Data.Table", "first")),
            List(
              extendedComponentGroup("Standard", "Base", "Data.Vector", "quux")
            )
          )
        ),
        config(
          "Standard",
          "Base",
          ComponentGroups(
            List(newComponentGroup("Data.Vector", "one", "two")),
            List(
              extendedComponentGroup(
                "Standard",
                "Table",
                "Data.Table",
                "second"
              )
            )
          )
        ),
        config(
          "user",
          "Vector_Utils",
          ComponentGroups(
            List(),
            List(
              extendedComponentGroup("Standard", "Base", "Data.Vector", "three")
            )
          )
        )
      )

      resolver.run(testPackages) shouldEqual Vector(
        libraryComponentGroup(
          "Standard",
          "Table",
          "Data.Table",
          "first",
          "second"
        ),
        libraryComponentGroup(
          "Standard",
          "Base",
          "Data.Vector",
          "one",
          "two",
          "quux",
          "three"
        )
      )
    }

    "skip component groups extending nothing" in {
      val resolver = new ComponentGroupsResolver
      val testPackages = Vector(
        config(
          "user",
          "Unnamed",
          ComponentGroups(List(newComponentGroup("Main", "main")), List())
        ),
        config(
          "Standard",
          "Base",
          ComponentGroups(
            List(newComponentGroup("Data.Vector", "one", "two")),
            List()
          )
        ),
        config(
          "user",
          "Vector_Utils",
          ComponentGroups(
            List(),
            List(
              extendedComponentGroup("Custom", "Lib", "Vector", "three")
            )
          )
        ),
        config(
          "user",
          "Other_Utils",
          ComponentGroups(
            List(),
            List(
              extendedComponentGroup("Standard", "Base", "Data.List", "four")
            )
          )
        )
      )

      resolver.run(testPackages) shouldEqual Vector(
        libraryComponentGroup("user", "Unnamed", "Main", "main"),
        libraryComponentGroup("Standard", "Base", "Data.Vector", "one", "two")
      )
    }
  }
}

object ComponentGroupsResolverSpec {

  /** Create a new config. */
  def config(
    namespace: String,
    name: String,
    componentGroups: ComponentGroups = ComponentGroups.empty
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
      componentGroups      = Right(componentGroups)
    )

  /** Create a new component group. */
  def newComponentGroup(
    group: String,
    exports: String*
  ): ComponentGroup =
    ComponentGroup(
      group   = GroupName(group),
      color   = None,
      icon    = None,
      exports = exports.map(Component(_, None))
    )

  /** Create a new extended component group. */
  def extendedComponentGroup(
    extendedLibraryNamespace: String,
    extendedLibraryName: String,
    extendedGroup: String,
    exports: String*
  ): ExtendedComponentGroup =
    ExtendedComponentGroup(
      group = GroupReference(
        LibraryName(extendedLibraryNamespace, extendedLibraryName),
        GroupName(extendedGroup)
      ),
      exports = exports.map(Component(_, None))
    )

  /** Create a new library component group. */
  def libraryComponentGroup(
    libraryNamespace: String,
    libraryName: String,
    groupName: String,
    exports: String*
  ): LibraryComponentGroup =
    LibraryComponentGroup(
      library = LibraryName(libraryNamespace, libraryName),
      name    = GroupName(groupName),
      color   = None,
      icon    = None,
      exports = exports.map(LibraryComponent(_, None))
    )
}
