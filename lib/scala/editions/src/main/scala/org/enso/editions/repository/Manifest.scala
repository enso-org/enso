package org.enso.editions.repository

import org.enso.editions.EditionName
import org.enso.yaml.{SnakeYamlDecoder, SnakeYamlEncoder}
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

  implicit val decoderSnake: SnakeYamlDecoder[Manifest] =
    new SnakeYamlDecoder[Manifest] {
      override def decode(node: Node): Either[Throwable, Manifest] =
        node match {
          case seqNode: SequenceNode =>
            val decoder = implicitly[SnakeYamlDecoder[Seq[EditionName]]]
            decoder.decode(seqNode).map(Manifest(_))
          case mappingNode: MappingNode if mappingNode.getValue.size() == 1 =>
            val editionsNode = mappingNode.getValue.get(0)
            (editionsNode.getKeyNode, editionsNode.getValueNode) match {
              case (keyNode: ScalarNode, seqNode: SequenceNode)
                  if keyNode.getValue == "editions" =>
                val decoder = implicitly[SnakeYamlDecoder[Seq[EditionName]]]
                decoder.decode(seqNode).map(Manifest(_))
              case _ =>
                Left(new YAMLException("Failed to decode editions"))
            }
          case _ =>
            Left(new YAMLException("Failed to decode editions"))
        }
    }

  implicit val encoderSnake: SnakeYamlEncoder[Manifest] =
    new SnakeYamlEncoder[Manifest] {
      override def encode(value: Manifest) = {
        val editionsEncoder = implicitly[SnakeYamlEncoder[Seq[EditionName]]]
        toMap("editions", editionsEncoder.encode(value.editions))
      }
    }

  /** The name of the manifest file that should be present at the root of
    * editions repository.
    */
  val filename = "manifest.yaml"
}
