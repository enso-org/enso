package org.enso.languageserver.libraries

import io.circe._
import io.circe.syntax._
import org.enso.editions.LibraryName
import org.enso.pkg.{
  Component,
  ComponentGroup,
  ComponentGroups,
  ExtendedComponentGroup,
  GroupName,
  Shortcut
}

/** The description of component groups provided by the package. This
  * representation is used in the JSONRPC API.
  *
  * @param newGroups the list of component groups provided by the package
  * @param extendedGroups the list of component groups that this package extends
  */
case class LibraryComponentGroups(
  newGroups: List[LibraryComponentGroup],
  extendedGroups: List[LibraryComponentGroup]
)
object LibraryComponentGroups {

  /** Create a [[LibraryComponentGroups]] from the provided [[ComponentGroups]].
    *
    * @param libraryName the library name defining these component groups
    * @param componentGroups the component groups to convert
    * @return the [[LibraryComponentGroups]] representation of the provided
    * component groups
    */
  def fromComponentGroups(
    libraryName: LibraryName,
    componentGroups: ComponentGroups
  ): LibraryComponentGroups =
    LibraryComponentGroups(
      componentGroups.newGroups.map(
        LibraryComponentGroup.fromComponentGroup(libraryName, _)
      ),
      componentGroups.extendedGroups.map(
        LibraryComponentGroup.fromExtendedComponentGroup
      )
    )

  /** Fields for use when serializing the [[LibraryComponentGroups]]. */
  private object Fields {
    val newGroups      = "newGroups"
    val extendedGroups = "extendedGroups"
  }

  /** [[Encoder]] instance for the [[LibraryComponentGroups]]. */
  implicit val encoder: Encoder[LibraryComponentGroups] = { componentGroups =>
    val newGroups = Option.unless(componentGroups.newGroups.isEmpty)(
      Fields.newGroups -> componentGroups.newGroups.asJson
    )
    val extendedGroups = Option.unless(componentGroups.extendedGroups.isEmpty)(
      Fields.extendedGroups -> componentGroups.extendedGroups.asJson
    )
    Json.obj(newGroups.toSeq ++ extendedGroups.toSeq: _*)
  }

  /** [[Decoder]] instance for the [[LibraryComponentGroups]]. */
  implicit val decoder: Decoder[LibraryComponentGroups] = { json =>
    for {
      newGroups <- json
        .getOrElse[List[LibraryComponentGroup]](Fields.newGroups)(List())
      extendedGroups <- json
        .getOrElse[List[LibraryComponentGroup]](Fields.extendedGroups)(List())
    } yield LibraryComponentGroups(newGroups, extendedGroups)
  }
}

/** The component group definition of a library. This representation is used in
  * the JSONRPC API.
  *
  * @param library the library name
  * @param group the group name
  * @param color the component group color
  * @param icon the component group icon
  * @param exports the list of components provided by this component group
  */
case class LibraryComponentGroup(
  library: LibraryName,
  group: GroupName,
  color: Option[String],
  icon: Option[String],
  exports: Seq[LibraryComponent]
)
object LibraryComponentGroup {

  /** create a [[LibraryComponentGroup]] from the provided [[ComponentGroup]].
    *
    * @param libraryName the library name defining this component group
    * @param componentGroup the component group to convert
    * @return the [[LibraryComponentGroup]] representation of this component
    * group
    */
  def fromComponentGroup(
    libraryName: LibraryName,
    componentGroup: ComponentGroup
  ): LibraryComponentGroup =
    LibraryComponentGroup(
      library = libraryName,
      group   = componentGroup.group,
      color   = componentGroup.color,
      icon    = componentGroup.icon,
      exports = componentGroup.exports.map(LibraryComponent.fromComponent)
    )

  /** Create a [[LibraryComponentGroup]] from the [[ExtendedComponentGroup]].
    *
    * @param extendedComponentGroup the extended component group to convert
    * @return the [[LibraryComponentGroup]] representation of the provided
    * extended component group
    */
  def fromExtendedComponentGroup(
    extendedComponentGroup: ExtendedComponentGroup
  ): LibraryComponentGroup =
    LibraryComponentGroup(
      library = extendedComponentGroup.group.libraryName,
      group   = extendedComponentGroup.group.groupName,
      color   = None,
      icon    = None,
      exports =
        extendedComponentGroup.exports.map(LibraryComponent.fromComponent)
    )

  /** Fields for use when serializing the [[LibraryComponentGroup]]. */
  private object Fields {
    val Library = "library"
    val Module  = "module"
    val Color   = "color"
    val Icon    = "icon"
    val Exports = "exports"
  }

  /** [[Encoder]] instance for the [[LibraryComponentGroup]]. */
  implicit val encoder: Encoder[LibraryComponentGroup] = { componentGroup =>
    val color = componentGroup.color.map(Fields.Color -> _.asJson)
    val icon  = componentGroup.icon.map(Fields.Icon -> _.asJson)
    val exports = Option.unless(componentGroup.exports.isEmpty)(
      Fields.Exports -> componentGroup.exports.asJson
    )
    Json.obj(
      (Fields.Library -> componentGroup.library.asJson) +:
      (Fields.Module  -> componentGroup.group.asJson) +:
      (color.toSeq ++ icon.toSeq ++ exports.toSeq): _*
    )
  }

  /** [[Decoder]] instance for the [[LibraryComponentGroup]]. */
  implicit val decoder: Decoder[LibraryComponentGroup] = { json =>
    for {
      library <- json.get[LibraryName](Fields.Library)
      module  <- json.get[GroupName](Fields.Module)
      color   <- json.get[Option[String]](Fields.Color)
      icon    <- json.get[Option[String]](Fields.Icon)
      exports <- json.getOrElse[List[LibraryComponent]](Fields.Exports)(List())
    } yield LibraryComponentGroup(library, module, color, icon, exports)
  }
}

/** A single component of a component group. This representation is used in
  * the JSONRPC API.
  *
  * @param name the component name
  * @param shortcut the component shortcut
  */
case class LibraryComponent(name: String, shortcut: Option[Shortcut])
object LibraryComponent {

  /** Create a  [[LibraryComponent]] from the provided [[Component]].
    *
    * @param component the component to convert
    * @return the [[LibraryComponent]] representation of this component
    */
  def fromComponent(component: Component): LibraryComponent =
    LibraryComponent(component.name, component.shortcut)

  object Fields {
    val Name     = "name"
    val Shortcut = "shortcut"
  }

  /** [[Encoder]] instance for the [[LibraryComponent]]. */
  implicit val encoder: Encoder[LibraryComponent] = { component =>
    val shortcut = component.shortcut.map(Fields.Shortcut -> _.asJson)
    Json.obj(
      Seq(Fields.Name -> component.name.asJson) ++
      shortcut.toSeq: _*
    )
  }

  /** [[Decoder]] instance for the [[LibraryComponent]]. */
  implicit val decoder: Decoder[LibraryComponent] = { json =>
    for {
      name     <- json.get[String](Fields.Name)
      shortcut <- json.getOrElse[Option[Shortcut]](Fields.Shortcut)(None)
    } yield LibraryComponent(name, shortcut)
  }
}
