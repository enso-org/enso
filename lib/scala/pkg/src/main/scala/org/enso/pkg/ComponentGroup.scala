package org.enso.pkg

import io.circe._
import io.circe.syntax._
import org.enso.editions.LibraryName

/** The description of component groups provided by the package.
  *
  * @param newGroups the list of component groups provided by the package
  * @param extendedGroups the list of component groups that this package extends
  */
case class ComponentGroups(
  newGroups: List[ComponentGroup],
  extendedGroups: List[ExtendedComponentGroup]
)
object ComponentGroups {

  /** Empty component groups. */
  val empty: ComponentGroups =
    ComponentGroups(List(), List())

  /** Fields for use when serializing the [[ComponentGroups]]. */
  private object Fields {
    val New     = "new"
    val Extends = "extends"
  }

  /** [[Encoder]] instance for the [[ComponentGroups]]. */
  implicit val encoder: Encoder[ComponentGroups] = { componentGroups =>
    val newGroups = Option.unless(componentGroups.newGroups.isEmpty)(
      Fields.New -> componentGroups.newGroups.asJson
    )
    val extendsGroups = Option.unless(componentGroups.extendedGroups.isEmpty)(
      Fields.Extends -> componentGroups.extendedGroups.asJson
    )
    Json.obj(newGroups.toSeq ++ extendsGroups.toSeq: _*)
  }

  /** [[Decoder]] instance for the [[ComponentGroups]]. */
  implicit val decoder: Decoder[ComponentGroups] = { json =>
    for {
      newGroups <- json.getOrElse[List[ComponentGroup]](Fields.New)(List())
      extendsGroups <-
        json.getOrElse[List[ExtendedComponentGroup]](Fields.Extends)(List())
    } yield ComponentGroups(newGroups, extendsGroups)
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
  module: ModuleName,
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
      name <- ConfigCodecs
        .getFromObject[ModuleName](
          "component group name",
          Fields.Module,
          json
        )
      color   <- json.get[Option[String]](Fields.Color)
      icon    <- json.get[Option[String]](Fields.Icon)
      exports <- json.getOrElse[List[Component]](Fields.Exports)(List())
    } yield ComponentGroup(name, color, icon, exports)
  }

}

/** The definition of a component group that extends an existing one.
  *
  * @param module the reference to the extended component group
  * @param color the component group color
  * @param icon the component group icon
  * @param exports the list of components provided by this component group
  */
case class ExtendedComponentGroup(
  module: ModuleReference,
  color: Option[String],
  icon: Option[String],
  exports: Seq[Component]
)
object ExtendedComponentGroup {

  /** Fields for use when serializing the [[ExtendedComponentGroup]]. */
  private object Fields {
    val Module  = "module"
    val Color   = "color"
    val Icon    = "icon"
    val Exports = "exports"
  }

  /** [[Encoder]] instance for the [[ExtendedComponentGroup]]. */
  implicit val encoder: Encoder[ExtendedComponentGroup] = {
    extendedComponentGroup =>
      val color = extendedComponentGroup.color.map(Fields.Color -> _.asJson)
      val icon  = extendedComponentGroup.icon.map(Fields.Icon -> _.asJson)
      val exports = Option.unless(extendedComponentGroup.exports.isEmpty)(
        Fields.Exports -> extendedComponentGroup.exports.asJson
      )
      Json.obj(
        (Fields.Module -> extendedComponentGroup.module.asJson) +:
        (color.toSeq ++ icon.toSeq ++ exports.toSeq): _*
      )
  }

  /** [[Decoder]] instance for the [[ExtendedComponentGroup]]. */
  implicit val decoder: Decoder[ExtendedComponentGroup] = { json =>
    for {
      reference <- ConfigCodecs
        .getFromObject[ModuleReference](
          "extended component group reference",
          Fields.Module,
          json
        )
      color   <- json.get[Option[String]](Fields.Color)
      icon    <- json.get[Option[String]](Fields.Icon)
      exports <- json.getOrElse[List[Component]](Fields.Exports)(List())
    } yield ExtendedComponentGroup(reference, color, icon, exports)
  }
}

/** A single component of a component group.
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
          name <- ConfigCodecs
            .getFromObject[String]("component name", Fields.Name, json)
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
    ConfigCodecs.getScalar("shortcut", json).map(Shortcut(_))
  }
}

/** The reference to a module.
  *
  * @param libraryName the qualified name of a library where the module is defined
  * @param moduleName the module name
  */
case class ModuleReference(
  libraryName: LibraryName,
  moduleName: Option[ModuleName]
)
object ModuleReference {

  private def toModuleString(moduleReference: ModuleReference): String = {
    val libraryName =
      s"${moduleReference.libraryName.namespace}${LibraryName.separator}${moduleReference.libraryName.name}"
    moduleReference.moduleName.fold(libraryName) { moduleName =>
      s"$libraryName${LibraryName.separator}${moduleName.name}"
    }
  }

  /** [[Encoder]] instance for the [[ModuleReference]]. */
  implicit val encoder: Encoder[ModuleReference] = { moduleReference =>
    toModuleString(moduleReference).asJson
  }

  /** [[Decoder]] instance for the [[ModuleReference]]. */
  implicit val decoder: Decoder[ModuleReference] = { json =>
    json.as[String].flatMap { moduleString =>
      moduleString.split(LibraryName.separator).toList match {
        case namespace :: name :: module =>
          Right(
            ModuleReference(
              LibraryName(namespace, name),
              ModuleName.fromComponents(module)
            )
          )
        case _ =>
          Left(
            DecodingFailure(
              s"Failed to decode '$moduleString' as module reference. " +
              s"Module reference should consist of a namespace (author), " +
              s"library name and a module name (e.g. Standard.Base.Data).",
              json.history
            )
          )
      }
    }
  }

}

/** The module name.
  *
  * @param name the module name
  */
case class ModuleName(name: String)
object ModuleName {

  def fromComponents(items: List[String]): Option[ModuleName] =
    Option.unless(items.isEmpty)(ModuleName(items.mkString(".")))

  /** [[Encoder]] instance for the [[ModuleName]]. */
  implicit val encoder: Encoder[ModuleName] = { moduleName =>
    moduleName.name.asJson
  }

  /** [[Decoder]] instance for the [[ModuleName]]. */
  implicit val decoder: Decoder[ModuleName] = { json =>
    json.as[String] match {
      case Left(_) =>
        Left(
          DecodingFailure(
            "Failed to decode component group name.",
            json.history
          )
        )
      case Right(name) =>
        Right(ModuleName(name))
    }
  }
}
