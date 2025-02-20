package org.enso.scala.yaml

import java.util

trait YamlEncoder[T] {

  def encode(value: T): Object

  /** Creates a single-element map from the provided values.
    */
  protected def toMap(
    key: String,
    value: Object
  ): java.util.Map[String, Object] = {
    val map = new util.LinkedHashMap[String, Object]()
    map.put(key, value)
    map
  }

  /** Creates a java.util.Map from the provided list of tuples, while preserving the order.
    * @param elements list of key-value pairs
    * @return a map
    */
  protected def toMap(
    elements: java.util.List[(String, Object)]
  ): java.util.Map[String, Object] = {
    val map: util.Map[String, Object] = new util.LinkedHashMap()
    elements.forEach { case (k, v) => map.put(k, v) }
    map
  }
}

object YamlEncoder {

  implicit def stringEncoderYaml: YamlEncoder[String] =
    new YamlEncoder[String] {
      override def encode(value: String): Object = {
        value
      }
    }

  implicit def booleanEncoderYaml: YamlEncoder[Boolean] =
    new YamlEncoder[Boolean] {
      override def encode(value: Boolean): Object = {
        value.toString
      }
    }

  implicit def iterableEncoderYaml[CC[X] <: IterableOnce[X], T](implicit
    elemEncoder: YamlEncoder[T]
  ): YamlEncoder[CC[T]] = new YamlEncoder[CC[T]] {
    override def encode(value: CC[T]): Object = {
      val elements = new util.ArrayList[Object](value.iterator.size)
      value.iterator.foreach(e => elements.add(elemEncoder.encode(e)))
      elements
    }
  }
}
