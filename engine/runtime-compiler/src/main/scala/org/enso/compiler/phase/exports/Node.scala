package org.enso.compiler.phase.exports

import org.enso.compiler.data.BindingsMap.ImportTarget

case class Node(
  target: ImportTarget
) {
  var exports: List[Edge]    = List()
  var exportedBy: List[Edge] = List()
}
