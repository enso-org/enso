package org.enso.compiler.dump;

import java.util.Map;
import java.util.Objects;

/**
 * Represents an edge in the GraphViz graph.
 *
 * @param from Identifier of the source node.
 * @param to Identifier of the target node.
 * @param label The label to display on the edge.
 * @param additionalAttrs Additional attributes to specify for the edge, except for label which is
 *     handled by the {@code label} field.
 */
record GraphVizEdge(String from, String to, String label, Map<String, String> additionalAttrs) {
  static GraphVizEdge newEdge(String from, String to) {
    Objects.requireNonNull(from);
    Objects.requireNonNull(to);
    return new GraphVizEdge(from, to, "", Map.of());
  }

  static GraphVizEdge newEdgeWithLabel(String from, String to, String label) {
    Objects.requireNonNull(from);
    Objects.requireNonNull(to);
    Objects.requireNonNull(label);
    return new GraphVizEdge(from, to, label, Map.of());
  }

  static GraphVizEdge newEdgeWithAttributes(
      String from, String to, String label, Map<String, String> attrs) {
    Objects.requireNonNull(from);
    Objects.requireNonNull(to);
    Objects.requireNonNull(label);
    Objects.requireNonNull(attrs);
    return new GraphVizEdge(from, to, label, attrs);
  }

  String toGraphViz() {
    var sb = new StringBuilder();
    sb.append(from).append(" -> ").append(to);
    if (!additionalAttrs.isEmpty()) {
      sb.append(" [");
      additionalAttrs.forEach(
          (k, v) -> {
            assert Utils.hasOneLine(k) : k;
            assert Utils.hasOneLine(v) : v;
            sb.append(k);
            sb.append("=");
            if (!Utils.isSurroundedByQuotes(v)) {
              sb.append("\"").append(v).append("\"");
            } else {
              sb.append(v);
            }
            sb.append(", ");
          });
      sb.append("label=\"");
    } else {
      sb.append(" [label=\"");
    }
    sb.append(label).append("\"];");
    return sb.toString();
  }
}
