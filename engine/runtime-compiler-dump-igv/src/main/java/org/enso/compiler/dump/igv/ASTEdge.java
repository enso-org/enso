package org.enso.compiler.dump.igv;

/**
 * Edge is unidirectional - from paretn to child
 *
 * @param node Pointer to child
 * @param label
 */
record ASTEdge(ASTNode node, String label) {
  enum EdgeType {
    EDGE_TYPE;
  }
}
