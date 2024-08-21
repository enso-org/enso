package org.enso.scala.yaml

import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes._

import scala.collection.{BuildFrom, mutable}
import scala.jdk.CollectionConverters.CollectionHasAsScala

abstract class YamlDecoder[T] {
  def decode(node: Node): Either[Throwable, T]

  final protected def mappingKV(mappingNode: MappingNode): Map[String, Node] = {
    val mutableMap = mutable.HashMap[String, Node]()
    val values     = mappingNode.getValue
    val len        = values.size()
    var i          = 0
    while (i < len) {
      val value = values.get(i)
      value.getKeyNode match {
        case n: ScalarNode =>
          mutableMap.put(n.getValue, value.getValueNode)
        case _: SequenceNode =>
          throw new YAMLException(
            "Expected a plain value as a map's key, got a sequence instead"
          )
        case _: MappingNode =>
          throw new YAMLException(
            "Expected a plain value as a map's key, got a map instead"
          )
      }
      i += 1
    }
    mutableMap.toMap
  }
}

object YamlDecoder {
  implicit def optionDecoderYaml[T](implicit
    valueDecoder: YamlDecoder[T]
  ): YamlDecoder[Option[T]] = new YamlDecoder[Option[T]] {
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
        valueDecoder
          .decode(mappingNode)
          .map(Option(_))
    }
  }

  /** Helper class used for automatic decoding of sequences of fields to a map.
    */
  trait MapKeyField {

    /** Determines the name of the field to be used as a key in the map.
      */
    def key: String

    /** Determines if duplicate map entries are allowed in the source YAML.
      */
    def duplicatesAllowed: Boolean
  }

  object MapKeyField {
    private case class PlainMapKeyField(key: String, duplicatesAllowed: Boolean)
        extends MapKeyField

    def plainField(
      key: String,
      duplicatesAllowed: Boolean = false
    ): MapKeyField = PlainMapKeyField(key, duplicatesAllowed)
  }

  implicit def mapDecoderYaml[K, V](implicit
    keyDecoder: YamlDecoder[K],
    valueDecoder: YamlDecoder[V],
    keyMapper: MapKeyField
  ): YamlDecoder[Map[K, V]] = new YamlDecoder[Map[K, V]] {
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
        val result = sequence
          .getValue()
          .asScala
          .toList
          .map(node =>
            node match {
              case mappingNode: MappingNode =>
                val kv = mappingKV(mappingNode)
                if (kv.contains(keyMapper.key)) {
                  for {
                    k <- keyDecoder
                      .decode(kv(keyMapper.key).asInstanceOf[ScalarNode])
                    v <- valueDecoder.decode(mappingNode)
                  } yield (k, v)
                } else {
                  Left(
                    new YAMLException(
                      s"Cannot find '${keyMapper.key}' in the list of fields "
                    )
                  )
                }
            }
          )
        val lifted = liftEither(result).map(_.toMap)
        if (
          lifted.isRight && lifted
            .map(_.size)
            .getOrElse(-1) != result.size && !keyMapper.duplicatesAllowed
        ) Left(new YAMLException("YAML definition contains duplicate entries"))
        else lifted
      case _ =>
        Left(new YAMLException("Expected `MappingNode` for a map value"))
    }

    def liftEither[A, B](xs: Seq[Either[A, B]]): Either[A, Seq[B]] = {
      xs.foldLeft[Either[A, Seq[B]]](Right(Seq.empty)) {
        case (acc @ Left(_), _)        => acc
        case (_, elem @ Left(_))       => elem.asInstanceOf[Either[A, Seq[B]]]
        case (Right(acc), Right(elem)) => Right(acc :+ elem)
      }
    }
  }

  implicit def stringDecoderYaml: YamlDecoder[String] =
    new YamlDecoder[String] {
      override def decode(node: Node): Either[Throwable, String] = {
        node match {
          case node: ScalarNode =>
            Right(node.getValue)
          case _: MappingNode =>
            Left(new YAMLException("Expected a plain value, got a map instead"))
          case _: SequenceNode =>
            Left(
              new YAMLException(
                "Expected a plain value, got a sequence instead"
              )
            )
        }
      }
    }

  implicit def booleanDecoderYaml: YamlDecoder[Boolean] =
    new YamlDecoder[Boolean] {
      override def decode(node: Node): Either[Throwable, Boolean] = {
        node match {
          case node: ScalarNode =>
            node.getValue match {
              case "true"  => Right(true)
              case "false" => Right(false)
              case v       => Left(new YAMLException("Unknown boolean value: " + v))
            }
          case _: MappingNode =>
            Left(new YAMLException("Expected a plain value, got a map instead"))
          case _: SequenceNode =>
            Left(
              new YAMLException(
                "Expected a plain value, got a sequence instead"
              )
            )
        }
      }
    }

  implicit def iterableDecoderYaml[CC[X] <: IterableOnce[X], T](implicit
    valueDecoder: YamlDecoder[T],
    cbf: BuildFrom[List[Either[Throwable, T]], T, CC[T]]
  ): YamlDecoder[CC[T]] = new YamlDecoder[CC[T]] {

    override def decode(node: Node): Either[Throwable, CC[T]] = node match {
      case seqNode: SequenceNode =>
        val elements = seqNode.getValue.asScala.map(valueDecoder.decode).toList
        liftEither(elements)(cbf)
      case _: ScalarNode =>
        Left(
          new YAMLException("Expected a sequence, got a plain value instead")
        )
      case _: MappingNode =>
        Left(new YAMLException("Expected a sequence, got a map instead"))
    }

    def liftEither[A, B](xs: List[Either[A, B]])(implicit
      cbf: BuildFrom[List[Either[A, B]], B, CC[B]]
    ): Either[A, CC[B]] = {
      val builder =
        xs.foldLeft[Either[A, scala.collection.mutable.Builder[B, CC[B]]]](
          Right(cbf.newBuilder(xs))
        ) {
          case (acc @ Left(_), _) => acc
          case (_, elem @ Left(_)) =>
            elem.asInstanceOf[
              Either[A, scala.collection.mutable.Builder[B, CC[B]]]
            ]
          case (Right(builder), Right(elem)) => Right(builder.addOne(elem))
        }

      builder.map(_.result())
    }
  }

}
