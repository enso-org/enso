package org.enso.pkg

import io.circe._
import io.circe.syntax._
import org.enso.editions.LibraryName
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node, ScalarNode, SequenceNode}
import org.enso.yaml.SnakeYamlDecoder

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

  implicit val yamlDecoder: SnakeYamlDecoder[ComponentGroups] =
    new SnakeYamlDecoder[ComponentGroups] {
      override def decode(node: Node): Either[Throwable, ComponentGroups] =
        node match {
          case mappingNode: MappingNode =>
            if (mappingNode.getValue.size() > 2)
              Left(
                new YAMLException(
                  "Invalid number of fields for ComponentGroups. Expected: 2"
                )
              )
            else {
              val clazzMap = mappingKV(mappingNode)
              val newGroupsDecoder =
                implicitly[SnakeYamlDecoder[List[ComponentGroup]]]
              val extendedGroupsDecoder =
                implicitly[SnakeYamlDecoder[List[ExtendedComponentGroup]]]
              for {
                newGroups <- clazzMap
                  .get(Fields.New)
                  .map(newGroupsDecoder.decode)
                  .getOrElse(Right(Nil))
                extendedGroups <- clazzMap
                  .get(Fields.Extends)
                  .map(extendedGroupsDecoder.decode)
                  .getOrElse(Right(Nil))
              } yield ComponentGroups(newGroups, extendedGroups)
            }
        }
    }
}

/** The definition of a single component group.
  *
  * @param group the group name
  * @param color the component group color
  * @param icon the component group icon
  * @param exports the list of components provided by this component group
  */
case class ComponentGroup(
  group: GroupName,
  color: Option[String],
  icon: Option[String],
  exports: Seq[Component]
)
object ComponentGroup {

  /** Fields for use when serializing the [[ComponentGroup]]. */
  private object Fields {
    val Group   = "group"
    val Color   = "color"
    val Icon    = "icon"
    val Exports = "exports"
  }

  implicit val decoderSnake: SnakeYamlDecoder[ComponentGroup] =
    new SnakeYamlDecoder[ComponentGroup] {
      override def decode(node: Node): Either[Throwable, ComponentGroup] =
        node match {
          case mappingNode: MappingNode =>
            if (mappingNode.getValue.size() > 4)
              Left(
                new YAMLException("Invalid number of fields for ComponentGroup")
              )
            else if (mappingNode.getValue.size() == 1) {
              val groupNode = mappingNode.getValue.get(0)
              (groupNode.getKeyNode, groupNode.getValueNode) match {
                case (scalarNode: ScalarNode, mappingNode: MappingNode) =>
                  val clazzMap     = mappingKV(mappingNode)
                  val groupDecoder = implicitly[SnakeYamlDecoder[GroupName]]
                  val colorDecoder =
                    implicitly[SnakeYamlDecoder[Option[String]]]
                  val iconDecoder =
                    implicitly[SnakeYamlDecoder[Option[String]]]
                  val exportDecoder =
                    implicitly[SnakeYamlDecoder[Seq[Component]]]

                  for {
                    group <- groupDecoder.decode(scalarNode)
                    color <- clazzMap
                      .get(Fields.Color)
                      .map(colorDecoder.decode)
                      .getOrElse(Right(None))
                    icon <- clazzMap
                      .get(Fields.Icon)
                      .map(iconDecoder.decode)
                      .getOrElse(Right(None))
                    exports <- clazzMap
                      .get(Fields.Exports)
                      .map(exportDecoder.decode)
                      .getOrElse(Right(Seq.empty))
                  } yield ComponentGroup(group, color, icon, exports)
                case (_: ScalarNode, value: ScalarNode) =>
                  Left(
                    new YAMLException(
                      "Failed to decode component group. Expected a mapping, got " + value.getValue
                    )
                  )
                case (_: ScalarNode, _: SequenceNode) =>
                  Left(
                    new YAMLException(
                      "Failed to decode component group. Expected a mapping, got a sequence"
                    )
                  )
                case _ =>
                  Left(
                    new YAMLException(
                      "Failed to decode component"
                    )
                  )
              }
            } else {
              Left(
                new YAMLException("Failed to decode component group")
              )
            }
        }
    }

  /** [[Encoder]] instance for the [[ComponentGroup]]. */
  implicit val encoder: Encoder[ComponentGroup] = { componentGroup =>
    val color = componentGroup.color.map(Fields.Color -> _.asJson)
    val icon  = componentGroup.icon.map(Fields.Icon -> _.asJson)
    val exports = Option.unless(componentGroup.exports.isEmpty)(
      Fields.Exports -> componentGroup.exports.asJson
    )
    Json.obj(
      componentGroup.group.name -> Json.obj(
        color.toSeq ++ icon.toSeq ++ exports.toSeq: _*
      )
    )
  }

  /** [[Decoder]] instance for the [[ComponentGroup]]. */
  implicit val decoder: Decoder[ComponentGroup] = { json =>
    for {
      name <- decodeName(json)
      componentGroup <- decodeComponentGroup(
        GroupName(name),
        json.downField(name)
      )
    } yield componentGroup
  }

  private def decodeName(cursor: ACursor): Decoder.Result[String] =
    ConfigCodecs
      .getNameFromKey(cursor)
      .toRight(decodingFailure(cursor.history))

  private def decodeComponentGroup(
    name: GroupName,
    cursor: ACursor
  ): Decoder.Result[ComponentGroup] = {
    if (cursor.keys.nonEmpty) {
      for {
        color   <- cursor.get[Option[String]](Fields.Color)
        icon    <- cursor.get[Option[String]](Fields.Icon)
        exports <- cursor.getOrElse[List[Component]](Fields.Exports)(List())
      } yield ComponentGroup(name, color, icon, exports)
    } else {
      Left(decodingFailure(cursor.history))
    }
  }

  private def decodingFailure(history: List[CursorOp]): DecodingFailure =
    DecodingFailure("Failed to decode component group", history)
}

/** The definition of a component group that extends an existing one.
  *
  * @param group the reference to the extended component group
  * @param exports the list of components provided by this component group
  */
case class ExtendedComponentGroup(
  group: GroupReference,
  exports: Seq[Component]
)
object ExtendedComponentGroup {

  /** Fields for use when serializing the [[ExtendedComponentGroup]]. */
  private object Fields {
    val Group   = "group"
    val Exports = "exports"
  }

  implicit val decoderSnake: SnakeYamlDecoder[ExtendedComponentGroup] =
    new SnakeYamlDecoder[ExtendedComponentGroup] {
      override def decode(
        node: Node
      ): Either[Throwable, ExtendedComponentGroup] = node match {
        case mappingNode: MappingNode =>
          if (mappingNode.getValue.size() > 2)
            Left(
              new YAMLException(
                "Invalid number of fields for ExtendedComponentGroup"
              )
            )
          else if (mappingNode.getValue.size() == 1) {
            val groupDecoder   = implicitly[SnakeYamlDecoder[GroupReference]]
            val exportsDecoder = implicitly[SnakeYamlDecoder[Seq[Component]]]
            val groupNode      = mappingNode.getValue.get(0)
            (groupNode.getKeyNode, groupNode.getValueNode) match {
              case (scalarNode: ScalarNode, seqNode: SequenceNode) =>
                for {
                  group   <- groupDecoder.decode(scalarNode)
                  exports <- exportsDecoder.decode(seqNode)
                } yield ExtendedComponentGroup(group, exports)
              case (groupNode: ScalarNode, componentExportsNode: MappingNode) =>
                val values      = componentExportsNode.getValue
                val valuesCount = values.size()
                if (valuesCount == 0) {
                  groupDecoder
                    .decode(groupNode)
                    .map(ExtendedComponentGroup(_, Seq.empty))
                } else if (valuesCount == 1) {
                  val exportsNode = values.get(0)
                  (exportsNode.getKeyNode, exportsNode.getValueNode) match {
                    case (exportsKeyNode: ScalarNode, seqNode: SequenceNode)
                        if exportsKeyNode.getValue == Fields.Exports =>
                      for {
                        group   <- groupDecoder.decode(groupNode)
                        exports <- exportsDecoder.decode(seqNode)
                      } yield ExtendedComponentGroup(group, exports)
                    case _ =>
                      Left(
                        new YAMLException(
                          "Failed to decode Extended ComponentGroup"
                        )
                      )
                  }
                } else {
                  Left(
                    new YAMLException(
                      "Failed to decode Extended Component Group"
                    )
                  )
                }
              case _ =>
                Left(
                  new YAMLException(
                    "Failed to decode Component Group's name in " + groupNode
                  )
                )
            }
          } else {
            Left(
              new YAMLException("Failed to decode Component Group's name")
            )
          }
        case scalarNode: ScalarNode =>
          val groupDecoder = implicitly[SnakeYamlDecoder[GroupReference]]
          groupDecoder
            .decode(scalarNode)
            .map(ExtendedComponentGroup(_, Seq.empty))
      }
    }

  /** [[Encoder]] instance for the [[ExtendedComponentGroup]]. */
  implicit val encoder: Encoder[ExtendedComponentGroup] = {
    extendedComponentGroup =>
      val exports = Option.unless(extendedComponentGroup.exports.isEmpty)(
        Fields.Exports -> extendedComponentGroup.exports.asJson
      )
      Json.obj(
        extendedComponentGroup.group.qualifiedName -> Json.obj(
          exports.toSeq: _*
        )
      )
  }

  /** [[Decoder]] instance for the [[ExtendedComponentGroup]]. */
  implicit val decoder: Decoder[ExtendedComponentGroup] = { json =>
    for {
      moduleName <- decodeModuleName(json)
      moduleReference <- GroupReference
        .fromModuleName(moduleName)
        .toRight(
          DecodingFailure(
            s"Failed to decode '$moduleName' as a module reference. " +
            s"Module reference should consist of a namespace (author), " +
            s"library name and a module name (e.g. Standard.Base.Data).",
            json.history
          )
        )
      componentGroup <-
        decodeExtendedComponentGroup(
          moduleReference,
          json.downField(moduleName)
        )
    } yield componentGroup
  }

  private def decodeModuleName(cursor: ACursor): Decoder.Result[String] =
    ConfigCodecs
      .getNameFromKey(cursor)
      .toRight(decodingFailure(cursor.history))

  private def decodeExtendedComponentGroup(
    reference: GroupReference,
    cursor: ACursor
  ): Decoder.Result[ExtendedComponentGroup] =
    if (cursor.keys.nonEmpty) {
      for {
        exports <- cursor.getOrElse[List[Component]](Fields.Exports)(List())
      } yield ExtendedComponentGroup(reference, exports)
    } else {
      Left(decodingFailure(cursor.history))
    }

  private def decodingFailure(history: List[CursorOp]): DecodingFailure =
    DecodingFailure("Failed to decode extended component group", history)

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

  implicit val decoderSnake: SnakeYamlDecoder[Component] =
    new SnakeYamlDecoder[Component] {
      override def decode(node: Node): Either[Throwable, Component] =
        node match {
          case mappingNode: MappingNode =>
            if (mappingNode.getValue.size() > 2)
              Left(new YAMLException("invalid number of fields for Component"))
            else if (mappingNode.getValue.size() == 1) {
              val componentNode = mappingNode.getValue.get(0)
              (componentNode.getKeyNode, componentNode.getValueNode) match {
                case (scalarNode: ScalarNode, mappingNode: MappingNode) =>
                  val stringDecoder = implicitly[SnakeYamlDecoder[String]]
                  val shortcutDecoder =
                    implicitly[SnakeYamlDecoder[Option[Shortcut]]]
                  for {
                    name <- stringDecoder.decode(scalarNode)
                    shortcut <- shortcutDecoder
                      .decode(mappingNode)
                      .map(_.filter(_.key.nonEmpty))
                  } yield Component(name, shortcut)
                case (keyNode: ScalarNode, _: ScalarNode) =>
                  Left(
                    new YAMLException(
                      "Failed to decode exported component '" + keyNode.getValue + "'"
                    )
                  )
                case _ =>
                  Left(
                    new YAMLException(
                      "Failed to decode component"
                    )
                  )
              }
            } else {
              Left(new YAMLException("Failed to decode Component"))
            }
          case scalarNode: ScalarNode =>
            val stringDecoder = implicitly[SnakeYamlDecoder[String]]
            stringDecoder.decode(scalarNode).map(Component(_, None))
        }
    }

  /** [[Encoder]] instance for the [[Component]]. */
  implicit val encoder: Encoder[Component] = { component =>
    component.shortcut match {
      case Some(shortcut) =>
        Json.obj(
          component.name -> Json.obj(
            Fields.Shortcut -> shortcut.asJson
          )
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
            .getNameFromKey(json)
            .toRight(decodingFailure(json.history))
          component <- decodeComponent(name, json.downField(name))
        } yield component
    }
  }

  private def decodeComponent(
    name: String,
    cursor: ACursor
  ): Decoder.Result[Component] = {
    if (cursor.keys.nonEmpty) {
      for {
        shortcut <- cursor.getOrElse[Option[Shortcut]](Fields.Shortcut)(None)
      } yield Component(name, shortcut)
    } else {
      Left(decodingFailure(cursor.history))
    }
  }

  private def decodingFailure(history: List[CursorOp]): DecodingFailure =
    DecodingFailure("Failed to decode exported component", history)
}

/** The shortcut reference to the component.
  *
  * @param key the shortcut key combination
  */
case class Shortcut(key: String)
object Shortcut {

  object Fields {
    val Key = "key"
  }

  implicit val decoderSnake: SnakeYamlDecoder[Shortcut] =
    new SnakeYamlDecoder[Shortcut] {
      override def decode(node: Node): Either[Throwable, Shortcut] =
        node match {
          case mappingNode: MappingNode =>
            val stringDecoder = implicitly[SnakeYamlDecoder[String]]

            if (mappingNode.getValue.size() != 1)
              Left(new YAMLException("Invalid number of fields for Shortcut"))
            else {
              val shortcutNode = mappingNode.getValue.get(0)
              (shortcutNode.getKeyNode, shortcutNode.getValueNode) match {
                case (key: ScalarNode, valueNode)
                    if key.getValue == "shortcut" =>
                  valueNode match {
                    case valueNode: ScalarNode =>
                      stringDecoder.decode(valueNode).map(Shortcut(_))
                    case _: SequenceNode =>
                      Left(
                        new YAMLException(
                          "Failed to decode shortcut. Expected a string value, got a sequence"
                        )
                      )
                    case _ =>
                      Left(new YAMLException("Failed to decode shortcut"))
                  }
                case _ =>
                  Left(new YAMLException("Failed to decode shortcut"))
              }
            }
        }
    }

  /** [[Encoder]] instance for the [[Shortcut]]. */
  implicit val encoder: Encoder[Shortcut] = { shortcut =>
    shortcut.key.asJson
  }

  /** [[Decoder]] instance for the [[Shortcut]]. */
  implicit val decoder: Decoder[Shortcut] = { json =>
    ConfigCodecs
      .getScalar(json)
      .map(Shortcut(_))
      .toRight(
        DecodingFailure("Failed to decode shortcut", json.history)
      )
  }
}

/** The reference to a component group.
  *
  * @param libraryName the qualified name of a library where the module is defined
  * @param groupName the module name
  */
case class GroupReference(
  libraryName: LibraryName,
  groupName: GroupName
) {

  /** The qualified name of the library consists of its prefix and name
    * separated with a dot.
    */
  def qualifiedName: String =
    s"$libraryName${LibraryName.separator}${groupName.name}"

  /** @inheritdoc */
  override def toString: String = qualifiedName

}
object GroupReference {

  /** Create a [[GroupReference]] from string. */
  def fromModuleName(moduleName: String): Option[GroupReference] =
    moduleName.split(LibraryName.separator).toList match {
      case namespace :: name :: module :: modules =>
        Some(
          GroupReference(
            LibraryName(namespace, name),
            GroupName.fromComponents(module, modules)
          )
        )
      case _ =>
        None
    }

  object Fields {
    val LibraryName = "library-name"
    val GroupName   = "group-name"
  }

  implicit val decoderSnake: SnakeYamlDecoder[GroupReference] =
    new SnakeYamlDecoder[GroupReference] {
      override def decode(node: Node): Either[Throwable, GroupReference] =
        node match {
          case mappingNode: MappingNode =>
            if (mappingNode.getValue.size() > 2)
              Left(
                new YAMLException("Invalid number of fields for GroupReference")
              )
            else {
              val clazzMap = mappingKV(mappingNode)

              val libraryNameDecoder = implicitly[SnakeYamlDecoder[LibraryName]]
              val groupNameDecoder   = implicitly[SnakeYamlDecoder[GroupName]]
              for {
                libraryName <- clazzMap
                  .get(Fields.LibraryName)
                  .toRight(
                    new YAMLException(s"Missing '${Fields.LibraryName}' field")
                  )
                  .flatMap(libraryNameDecoder.decode)
                groupName <- clazzMap
                  .get(Fields.GroupName)
                  .toRight(
                    new YAMLException(s"Missing '${Fields.GroupName}' field")
                  )
                  .flatMap(groupNameDecoder.decode)
              } yield GroupReference(libraryName, groupName)
            }
          case scalarNode: ScalarNode =>
            fromModuleName(scalarNode.getValue).toRight(
              new YAMLException(
                s"Failed to decode '${scalarNode.getValue}' as a module reference"
              )
            )
        }
    }
}

/** The module name.
  *
  * @param name the module name
  */
case class GroupName(name: String)
object GroupName {

  object Fields {
    val Name = "name"
  }

  implicit val decoderSnake: SnakeYamlDecoder[GroupName] =
    new SnakeYamlDecoder[GroupName] {
      override def decode(node: Node): Either[Throwable, GroupName] =
        node match {
          case mappingNode: MappingNode =>
            val stringDecoder = implicitly[SnakeYamlDecoder[String]]

            if (mappingNode.getValue.size() != 1)
              Left(new YAMLException("Invalid number of fields for GroupName"))
            else {
              val clazzMap = mappingKV(mappingNode)
              clazzMap
                .get(Fields.Name)
                .toRight(new YAMLException(s"Missing '${Fields.Name}' field"))
                .flatMap(stringDecoder.decode)
                .map(GroupName(_))
            }
          case scalarNode: ScalarNode =>
            val stringDecoder = implicitly[SnakeYamlDecoder[String]]
            stringDecoder.decode(scalarNode).map(GroupName(_))
        }
    }

  /** Create a [[GroupName]] from its components. */
  def fromComponents(item: String, items: List[String]): GroupName =
    GroupName((item :: items).mkString(LibraryName.separator.toString))

  /** [[Encoder]] instance for the [[GroupName]]. */
  implicit val encoder: Encoder[GroupName] = { moduleName =>
    moduleName.name.asJson
  }

  /** [[Decoder]] instance for the [[GroupName]]. */
  implicit val decoder: Decoder[GroupName] = { json =>
    json.as[String] match {
      case Left(_) =>
        Left(
          DecodingFailure(
            "Failed to decode component group name.",
            json.history
          )
        )
      case Right(name) =>
        Right(GroupName(name))
    }
  }
}
