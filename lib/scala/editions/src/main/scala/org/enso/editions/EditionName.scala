package org.enso.editions

import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{Node, ScalarNode, Tag}

/** A helper type to handle special parsing logic of edition names.
  *
  * The issue is that if an edition is called `2021.4` and it is written
  * unquoted inside of a YAML file, that is treated as a floating point
  * number, so special care must be taken to correctly parse it.
  */
case class EditionName(name: String) extends AnyVal {

  /** @inheritdoc */
  override def toString: String = name

  /** Returns the name of the file that is associated with the edition name. */
  def toFileName: String = name + EditionName.editionSuffix
}

object EditionName {

  /** A helper method for constructing an [[EditionName]]. */
  def apply(name: String): EditionName = new EditionName(name)

  implicit val yamlDecoder: YamlDecoder[EditionName] =
    new YamlDecoder[EditionName] {
      override def decode(node: Node): Either[Throwable, EditionName] =
        node match {
          case scalarNode: ScalarNode =>
            scalarNode.getTag match {
              case Tag.NULL =>
                Left(new YAMLException("edition cannot be empty"))
              case _ =>
                val stringDecoder = implicitly[YamlDecoder[String]]
                stringDecoder.decode(scalarNode).map(EditionName(_))
            }
          case _ =>
            Left(new YAMLException("unexpected edition name"))
        }
    }

  implicit val yamlEncoder: YamlEncoder[EditionName] =
    new YamlEncoder[EditionName] {
      override def encode(value: EditionName): Object = {
        value.name
      }
    }

  /** The filename suffix that is used to create a filename corresponding to a
    * named edition.
    */
  val editionSuffix = ".yaml"

  /** Creates an [[EditionName]] from the corresponding filename.
    *
    * Returns None if the filename does not correspond to an edition.
    */
  def fromFilename(filename: String): Option[EditionName] =
    if (filename.endsWith(editionSuffix))
      Some(EditionName(filename.stripSuffix(editionSuffix)))
    else None
}
