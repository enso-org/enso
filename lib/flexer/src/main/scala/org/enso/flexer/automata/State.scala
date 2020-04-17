package org.enso.flexer.automata

import com.google.common.{collect => guava}

import scala.collection.mutable

class State {
  val links: State.Link.Registry = new State.Link.Registry()
  var rule: Option[String]       = None
}

object State {

  object Implicits {

    implicit final class RangeMapWrapper[K <: Comparable[_], V](
      underlying: guava.RangeMap[K, V]
    ) {

      def getOption(key: K): Option[V] =
        Option(underlying.get(key))

      def isEmpty: Boolean =
        underlying.asMapOfRanges().isEmpty()
    }
  }

  val missing = -1

  case class Desc(priority: Int, rule: String)

  object Link {

    class Registry {
      val epsilon: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer()
      val ranged: guava.RangeMap[java.lang.Integer, Int] =
        guava.TreeRangeMap.create[java.lang.Integer, Int]()

      def add(target: Int): Unit =
        epsilon += target

      def add(target: Int, range: Range) =
        if (range.start <= range.end)
          ranged.put(guava.Range.closed(range.start, range.end), target)
    }
  }
}
