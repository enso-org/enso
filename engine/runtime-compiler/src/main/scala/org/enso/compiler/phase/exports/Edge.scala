package org.enso.compiler.phase.exports

case class Edge(
  exporter: Node,
  exportsAs: Option[String],
  exportee: Node
)
