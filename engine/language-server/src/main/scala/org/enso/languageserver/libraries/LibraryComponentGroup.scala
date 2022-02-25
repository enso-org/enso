package org.enso.languageserver.libraries

import io.circe._
import io.circe.syntax._
import org.enso.editions.LibraryName
import org.enso.pkg.{Component, ModuleName}

/** The component group definition of a library.
  *
  * @param library the library name
  * @param module the module name
  * @param color the component group color
  * @param icon the component group icon
  * @param exports the list of components provided by this component group
  */
case class LibraryComponentGroup(
  library: LibraryName,
  module: ModuleName,
  color: Option[String],
  icon: Option[String],
  exports: Seq[Component]
)
object LibraryComponentGroup {

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
      (Fields.Module  -> componentGroup.module.asJson) +:
      (color.toSeq ++ icon.toSeq ++ exports.toSeq): _*
    )
  }

  /** [[Decoder]] instance for the [[LibraryComponentGroup]]. */
  implicit val decoder: Decoder[LibraryComponentGroup] = { json =>
    for {
      library <- json.get[LibraryName](Fields.Library)
      module  <- json.get[ModuleName](Fields.Module)
      color   <- json.get[Option[String]](Fields.Color)
      icon    <- json.get[Option[String]](Fields.Icon)
      exports <- json.getOrElse[List[Component]](Fields.Exports)(List())
    } yield LibraryComponentGroup(library, module, color, icon, exports)
  }

}
