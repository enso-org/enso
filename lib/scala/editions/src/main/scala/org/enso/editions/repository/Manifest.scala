package org.enso.editions.repository

import org.enso.editions.EditionName
import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node, ScalarNode, SequenceNode}

/** The Edition Repository manifest, which lists all editions that the
  * repository provides.
  */
case class Manifest(editions: Seq[EditionName])

object Manifest {
  object Fields {
    val editions = "editions"
  }

  implicit val yamlDecoder: YamlDecoder[Manifest] =
    new YamlDecoder[Manifest] {
      override def decode(node: Node): Either[Throwable, Manifest] =
        node match {
          case seqNode: SequenceNode =>
            val decoder = implicitly[YamlDecoder[Seq[EditionName]]]
            decoder.decode(seqNode).map(Manifest(_))
          case mappingNode: MappingNode if mappingNode.getValue.size() == 1 =>
            val editionsNode = mappingNode.getValue.get(0)
            (editionsNode.getKeyNode, editionsNode.getValueNode) match {
              case (keyNode: ScalarNode, seqNode: SequenceNode)
                  if keyNode.getValue == Fields.editions =>
                val decoder = implicitly[YamlDecoder[Seq[EditionName]]]
                decoder.decode(seqNode).map(Manifest(_))
              case _ =>
                Left(new YAMLException("Failed to decode editions"))
            }
          case _ =>
            Left(new YAMLException("Failed to decode editions"))
        }
    }

  implicit val yamlEncoder: YamlEncoder[Manifest] =
    new YamlEncoder[Manifest] {
      override def encode(value: Manifest) = {
        val editionsEncoder = implicitly[YamlEncoder[Seq[EditionName]]]
        toMap(Fields.editions, editionsEncoder.encode(value.editions))
      }
    }

  /** The name of the manifest file that should be present at the root of
    * editions repository.
    */
  val filename = "manifest.yaml"
}
