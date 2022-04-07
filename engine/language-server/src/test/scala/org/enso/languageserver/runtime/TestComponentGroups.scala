package org.enso.languageserver.runtime

import org.enso.editions.LibraryName
import org.enso.pkg.{
  Component,
  ComponentGroup,
  ComponentGroups,
  ExtendedComponentGroup,
  ModuleName,
  ModuleReference
}

/** An object holding component groups required for tests. */
object TestComponentGroups {

  val standardBase: Map[LibraryName, ComponentGroups] = Map(
    LibraryName("Standard", "Base") -> ComponentGroups(
      List(
        ComponentGroup(
          ModuleName("Input"),
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
          ModuleReference(LibraryName("Standard", "Base"), ModuleName("Input")),
          Seq(Component("Standard.Database.Connection.Database.connect", None))
        )
      )
    )
  )

}
