package org.enso.compiler.phase.exports

case class Edge(
  exporter: Node,
  symbols: List[String],
  exportsAs: Option[String],
  exportee: Node
)
