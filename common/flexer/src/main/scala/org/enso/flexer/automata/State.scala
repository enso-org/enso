package org.enso.flexer.automata

import org.feijoas.mango.common.{collect => Guava}

import scala.collection.mutable

class State {
  val links: State.Link.Registry = new State.Link.Registry()
  var rule: Option[String]       = None
}

object State {
  val missing = -1

  case class Desc(priority: Int, rule: String)

  object Link {
    class Registry {
      private type IntOrd = Ordering.Int.type
      val epsilon: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer()
      val ranged: Guava.mutable.RangeMap[Int, Int, IntOrd] =
        Guava.mutable.RangeMap()

      def add(target: Int): Unit =
        epsilon += target

      def add(target: Int, range: Range) =
        if (range.start <= range.end)
          ranged.put(Guava.Range.closed(range.start, range.end), target)
    }
  }
}
