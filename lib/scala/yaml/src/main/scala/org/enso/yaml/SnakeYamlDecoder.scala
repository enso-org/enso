package org.enso.yaml

import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.ScalarNode
import org.yaml.snakeyaml.nodes.MappingNode
import org.yaml.snakeyaml.nodes.SequenceNode
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.error.YAMLException

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.collection.BuildFrom

abstract class SnakeYamlDecoder[T] {
  def decode(node: Node): Either[Throwable, T]

  protected def mappingKV(mappingNode: MappingNode): Map[String, Node] = {
    mappingNode.getValue.asScala.map { node =>
      node.getKeyNode match {
        case n: ScalarNode =>
          (n.getValue, node.getValueNode)
        case _: SequenceNode =>
          throw new YAMLException("expected a plain value as a map's key, got a sequence instead")
        case _: MappingNode =>
          throw new YAMLException("expected a plain value as a map's key, got a map instead")

      }
    }.toMap
  }
}

object SnakeYamlDecoder {
  implicit def optionDecoderYaml[T](implicit valueDecoder: SnakeYamlDecoder[T]): SnakeYamlDecoder[Option[T]] = new SnakeYamlDecoder[Option[T]] {
    override def decode(node: Node): Either[Throwable, Option[T]] = node match {
      case node: ScalarNode =>
        node.getTag match {
          case Tag.NULL => Right(None)
          case _ =>
            val v = node.getValue
            if (v == null || v.isEmpty) Right(None)
            else valueDecoder.decode(node).map(Some(_))
        }
      case mappingNode: MappingNode =>
        valueDecoder.decode(mappingNode).map(v => if (v == null) None else Some(v))
    }
  }

  trait MapKeyField {
    def key: String
  }

  object MapKeyField {
    private case class PlainMapKeyField(key: String) extends MapKeyField

    def plainField(key: String): MapKeyField = PlainMapKeyField(key)
  }

  implicit def mapDecoderYaml[K, V](implicit keyDecoder: SnakeYamlDecoder[K], valueDecoder: SnakeYamlDecoder[V], keyMapper: MapKeyField): SnakeYamlDecoder[Map[K, V]] = new SnakeYamlDecoder[Map[K, V]] {
    override def decode(node: Node): Either[Throwable, Map[K, V]] = node match {
      case mapping: MappingNode =>
        val kv = mapping.getValue.asScala.map { node =>
          for {
            k <- keyDecoder.decode(node.getKeyNode)
            v <- valueDecoder.decode(node.getValueNode)
          } yield (k, v)
        }
        liftEither(kv.toSeq).map(_.toMap)
      case sequence: SequenceNode =>
        val result = sequence.getValue().asScala.toList.map ( node =>
          node match {
            case mappingNode: MappingNode =>
              val kv = mappingKV(mappingNode)
              if (kv.contains(keyMapper.key)) {
                for {
                  k <- keyDecoder.decode(kv(keyMapper.key).asInstanceOf[ScalarNode])
                  v <- valueDecoder.decode(mappingNode)
                } yield (k, v)
              } else {
                Left(new YAMLException(s"Cannot find '${keyMapper.key}' in the list of fields "))
              }
          }
        )
        liftEither(result).map(_.toMap)
      case _ =>
        Left(new YAMLException("Expected `MappingNode` for a map value"))
    }

    def liftEither[A, B](xs: Seq[Either[A, B]]): Either[A, Seq[B]] = {
      xs.foldLeft[Either[A, Seq[B]]](Right(Seq.empty)) {
        case (acc@Left(_), _) => acc
        case (_, elem@Left(_)) => elem.asInstanceOf[Either[A, Seq[B]]]
        case (Right(acc), Right(elem)) => Right(acc :+ elem)
      }
    }
  }

  implicit def stringDecoderYaml: SnakeYamlDecoder[String] = new SnakeYamlDecoder[String] {
    override def decode(node: Node): Either[Throwable, String] = {
      node match {
        case node: ScalarNode =>
          Right(node.getValue)
        case _ =>
          Left(new YAMLException("Expected `ScalarNode` for a string value"))
      }
    }
  }

  implicit def booleanDecoderYaml: SnakeYamlDecoder[Boolean] = new SnakeYamlDecoder[Boolean] {
    override def decode(node: Node): Either[Throwable, Boolean] = {
      node match {
        case node: ScalarNode =>
          node.getValue match {
            case "true" => Right(true)
            case "false" => Right(false)
            case v => Left(new YAMLException("unknown boolean value: " + v))
          }
        case _ =>
          Left(new YAMLException("failed to decode a boolean field"))
      }
    }
  }

  implicit def iterableDecoderYaml[CC[X] <: IterableOnce[X], T](implicit valueDecoder: SnakeYamlDecoder[T], cbf: BuildFrom[List[Either[Throwable, T]], T, CC[T]]): SnakeYamlDecoder[CC[T]] = new SnakeYamlDecoder[CC[T]] {

    override def decode(node: Node): Either[Throwable, CC[T]] = node match {
      case seqNode: SequenceNode =>
        val elements = seqNode.getValue.asScala.map(valueDecoder.decode).toList
        liftEither(elements)(cbf)
      case _: ScalarNode =>
        Left(new YAMLException("expected a sequence, got a plain value instead"))
      case _: MappingNode =>
        Left(new YAMLException("expected a sequence, got a map instead"))
    }

    def liftEither[A, B](xs: List[Either[A, B]])(implicit cbf: BuildFrom[List[Either[A, B]], B, CC[B]]): Either[A, CC[B]] = {
      val builder = xs.foldLeft[Either[A, scala.collection.mutable.Builder[B, CC[B]]]](Right(cbf.newBuilder(xs))) {
        case (acc@Left(_), _) => acc
        case (_, elem@Left(_)) => elem.asInstanceOf[Either[A, scala.collection.mutable.Builder[B, CC[B]]]]
        case (Right(builder), Right(elem)) => Right(builder.addOne(elem))
      }

      builder.map(_.result())
    }
  }

}
