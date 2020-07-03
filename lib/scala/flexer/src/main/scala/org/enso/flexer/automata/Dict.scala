package org.enso.flexer.automata

import scala.collection.immutable

final class Dict extends Iterable[(Range, Int)] {
  private var divisions = immutable.SortedSet[Int](0, Int.MaxValue)

  def insert(range: Range): Unit = {
    divisions = divisions + range.start
    divisions = divisions + (range.end + 1)
  }

  override def size: Int =
    divisions.size - 1

  override def iterator: Iterator[(Range, Int)] =
    divisions.iterator.zip(divisions.iterator.drop(1)).zipWithIndex.map {
      case ((start, end), ix) => (start until end, ix)
    }

  override def toString: String =
    "Dict(" + divisions.toList.map(_.toString).mkString(",") + ")"
}
