package org.enso.compiler.phase.exports

import org.enso.compiler.data.BindingsMap.ResolvedModule

case class Node(
  module: ResolvedModule
) {
  var exports: List[Edge]    = List()
  var exportedBy: List[Edge] = List()
}
