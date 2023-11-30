package org.enso.compiler.core.ir

import scala.collection.immutable.StrictOptimizedMapOps
import scala.collection.immutable.Map
import scala.collection.Iterator

final private class IrLazyImMap[K, V](
  val underlying: java.util.Map[K, V]
) extends Map[K, V]
    with StrictOptimizedMapOps[K, V, Map, Map[K, V]] {
  def removed(key: K) =
    new scala.jdk.CollectionConverters.MapHasAsScala(underlying).asScala.toMap
      .removed(key)

  def updated[V1 >: V](key: K, value: V1) =
    new scala.jdk.CollectionConverters.MapHasAsScala(underlying).asScala.toMap
      .updated(key, value)

  def get(key: K) = Option(underlying.get(key))

  def iterator = new Iterator[(K, V)]() {
    val keys             = underlying.entrySet().iterator()
    def hasNext: Boolean = keys.hasNext()
    def next(): (K, V) = {
      val entry = keys.next()
      (entry.getKey, entry.getValue)
    }
  }

  override def size = underlying.size

  // Has to override the `contains` method because the underlying implementation
  // calls `get(key)`
  override def contains(key: K): Boolean = {
    underlying.containsKey(key)
  }
}
