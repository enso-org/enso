package org.enso.flexer.automata

import scala.collection.mutable

case class DFA(
  vocabulary: Dict,
  links: Array[Array[Int]],
  endStatePriorityMap: mutable.Map[Int, State.Desc]
)
