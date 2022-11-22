package org.enso.languageserver.runtime

import org.enso.editions.LibraryName
import org.enso.pkg.{
  Component,
  ComponentGroup,
  ComponentGroups,
  ExtendedComponentGroup,
  GroupName,
  GroupReference,
  Shortcut
}

/** An object holding component groups required for tests. */
object TestComponentGroups {

  val testLibraryComponentGroups: ComponentGroups =
    ComponentGroups(
      newGroups = List(
        ComponentGroup(
          group = GroupName("Foo"),
          color = Some("#32a852"),
          icon  = None,
          exports = Seq(
            Component("foo", Some(Shortcut("abc"))),
            Component("bar", None)
          )
        )
      ),
      extendedGroups = List(
        ExtendedComponentGroup(
          group = GroupReference(
            LibraryName("Standard", "Base"),
            GroupName("Data")
          ),
          exports = List(
            Component("bar", None)
          )
        )
      )
    )

  val standardBase: Map[LibraryName, ComponentGroups] = Map(
    LibraryName("Standard", "Base") -> ComponentGroups(
      List(
        ComponentGroup(
          GroupName("Input"),
          None,
          None,
          Seq(Component("Standard.Base.File.new", None))
        )
      ),
      List()
    ),
    LibraryName("Standard", "Database") -> ComponentGroups(
      List(),
      List(
        ExtendedComponentGroup(
          GroupReference(LibraryName("Standard", "Base"), GroupName("Input")),
          Seq(Component("Standard.Database.Connection.Database.connect", None))
        )
      )
    )
  )

}
