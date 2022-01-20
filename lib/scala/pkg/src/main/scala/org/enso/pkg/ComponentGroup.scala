package org.enso.pkg

import io.circe._
import io.circe.syntax._

/** The description of component groups provided by the package.
  *
  * @param `new` the list of component groups provided by the package
  * @param `extends` the list of component groups that this package extends
  */
case class ComponentGroups(
  `new`: List[ComponentGroup],
  `extends`: List[ComponentGroup]
)
object ComponentGroups {

  /** An empty component groups. */
  val empty: ComponentGroups =
    ComponentGroups(List(), List())

  /** Fields for use when serializing the [[ComponentGroups]]. */
  private object Fields {
    val New     = "new"
    val Extends = "extends"
  }

  /** Does the provided JSON object have unknown fields. */
  private def hasUnknownKeys(cursor: HCursor): Boolean =
    (ConfigCodecs.objectKeys(cursor) - Fields.New - Fields.Extends).nonEmpty

  /** [[Encoder]] instance for the [[ComponentGroups]]. */
  implicit val encoder: Encoder[ComponentGroups] = { componentGroups =>
    val newGroups = Option.unless(componentGroups.`new`.isEmpty)(
      Fields.New -> componentGroups.`new`.asJson
    )
    val extendsGroups = Option.unless(componentGroups.`extends`.isEmpty)(
      Fields.Extends -> componentGroups.`extends`.asJson
    )
    Json.obj(newGroups.toSeq ++ extendsGroups.toSeq: _*)
  }

  /** [[Decoder]] instance for the [[ComponentGroups]]. */
  implicit val decoder: Decoder[ComponentGroups] = { json =>
    def decodeComponentGroups: Either[DecodingFailure, ComponentGroups] = for {
      newGroups <- json.getOrElse[List[ComponentGroup]](Fields.New)(List())
      extendsGroups <- json.getOrElse[List[ComponentGroup]](Fields.Extends)(
        List()
      )
    } yield ComponentGroups(newGroups, extendsGroups)

    if (hasUnknownKeys(json)) {
      Left(
        DecodingFailure(
          "Invalid component groups. Valid keys are 'new' or 'extends'.",
          json.history
        )
      )
    } else {
      decodeComponentGroups
    }
  }
}

/** The definition of a single component group.
  *
  * @param module the module name
  * @param color the component group color
  * @param icon the component group icon
  * @param exports the list of components provided by this component group
  */
case class ComponentGroup(
  module: String,
  color: Option[String],
  icon: Option[String],
  exports: Seq[Component]
)
object ComponentGroup {

  /** Fields for use when serializing the [[ComponentGroup]]. */
  private object Fields {
    val Module  = "module"
    val Color   = "color"
    val Icon    = "icon"
    val Exports = "exports"
  }

  /** [[Encoder]] instance for the [[ComponentGroup]]. */
  implicit val encoder: Encoder[ComponentGroup] = { componentGroup =>
    val color = componentGroup.color.map(Fields.Color -> _.asJson)
    val icon  = componentGroup.icon.map(Fields.Icon -> _.asJson)
    val exports = Option.unless(componentGroup.exports.isEmpty)(
      Fields.Exports -> componentGroup.exports.asJson
    )
    Json.obj(
      (Fields.Module -> componentGroup.module.asJson) +:
      (color.toSeq ++ icon.toSeq ++ exports.toSeq): _*
    )
  }

  /** [[Decoder]] instance for the [[ComponentGroup]]. */
  implicit val decoder: Decoder[ComponentGroup] = { json =>
    for {
      name    <- ConfigCodecs.getName("component group")(json)
      color   <- json.get[Option[String]](Fields.Color)
      icon    <- json.get[Option[String]](Fields.Icon)
      exports <- json.getOrElse[List[Component]](Fields.Exports)(List())
    } yield ComponentGroup(name, color, icon, exports)
  }

}

/** A component of a component group.
  *
  * @param name the component name
  * @param shortcut the component shortcut
  */
case class Component(name: String, shortcut: Option[Shortcut])
object Component {

  object Fields {
    val Name     = "name"
    val Shortcut = "shortcut"
  }

  /** [[Encoder]] instance for the [[Component]]. */
  implicit val encoder: Encoder[Component] = { component =>
    component.shortcut match {
      case Some(shortcut) =>
        Json.obj(
          Fields.Name     -> component.name.asJson,
          Fields.Shortcut -> shortcut.asJson
        )
      case None =>
        component.name.asJson
    }
  }

  /** [[Decoder]] instance for the [[Component]]. */
  implicit val decoder: Decoder[Component] = { json =>
    json.as[String] match {
      case Right(name) =>
        Right(Component(name, None))
      case Left(_) =>
        for {
          name     <- ConfigCodecs.getName("component")(json)
          shortcut <- json.getOrElse[Option[Shortcut]](Fields.Shortcut)(None)
        } yield Component(name, shortcut)
    }
  }
}

/** The shortcut reference to the component.
  *
  * @param key the shortcut key combination
  */
case class Shortcut(key: String)
object Shortcut {

  /** [[Encoder]] instance for the [[Shortcut]]. */
  implicit val encoder: Encoder[Shortcut] = { shortcut =>
    shortcut.key.asJson
  }

  /** [[Decoder]] instance for the [[Shortcut]]. */
  implicit val decoder: Decoder[Shortcut] = { json =>
    ConfigCodecs.getScalar("shortcut")(json).map(Shortcut(_))
  }
}
