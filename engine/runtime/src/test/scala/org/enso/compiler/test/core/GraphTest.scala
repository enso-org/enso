package org.enso.compiler.test.core

import org.enso.compiler.core.Core
import org.enso.compiler.test.CompilerTest
import org.enso.graph.{Graph => PrimGraph}

class GraphTest extends CompilerTest {
  import Core._
  implicit val graph: PrimGraph.GraphData[Core.CoreGraph] = PrimGraph[Core.CoreGraph]()
}
