package org.enso.compiler.core.ir

import scala.collection.immutable.AbstractMap
import scala.collection.immutable.Map
import scala.collection.immutable.MapOps
import scala.collection.IterableFactory
import scala.collection.immutable.Iterable
import scala.collection.MapFactory
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder

final private class IrLazyImMap[K, +V](
  val underlying: scala.collection.Map[K, V],
  val defaultValue: K => V
) extends AbstractMap[K, V]
    with MapOps[K, V, Map, IrLazyImMap[K, V]] {

  def get(key: K): Option[V] = underlying.get(key)

  override def default(key: K): V = defaultValue(key)

  override def iterableFactory: IterableFactory[Iterable] =
    underlying.iterableFactory

  def iterator: Iterator[(K, V)] = underlying.iterator

  override def isEmpty: Boolean = underlying.isEmpty

  override def mapFactory: MapFactory[Map] = underlying.mapFactory

  override def concat[V2 >: V](
    xs: collection.IterableOnce[(K, V2)]
  ): IrLazyImMap[K, V2] =
    new IrLazyImMap(underlying.concat(xs), defaultValue)

  def removed(key: K): IrLazyImMap[K, V] =
    new IrLazyImMap[K, V](underlying.removed(key), defaultValue)

  def updated[V1 >: V](key: K, value: V1): IrLazyImMap[K, V1] =
    new IrLazyImMap[K, V1](underlying.updated(key, value), defaultValue)

  override def empty: IrLazyImMap[K, V] =
    new IrLazyImMap[K, V](underlying.empty, defaultValue)

  override protected def fromSpecific(
    coll: collection.IterableOnce[(K, V)] @uncheckedVariance
  ): IrLazyImMap[K, V] =
    new IrLazyImMap[K, V](mapFactory.from(coll), defaultValue)

  override protected def newSpecificBuilder
    : Builder[(K, V), IrLazyImMap[K, V]] @uncheckedVariance =
    Map.newBuilder.mapResult((p: Map[K, V]) =>
      new IrLazyImMap[K, V](p, defaultValue)
    )
}
